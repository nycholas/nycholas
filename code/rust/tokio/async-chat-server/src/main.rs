use std::collections::HashMap;
use std::io;
use std::net::SocketAddr;
use std::sync::Arc;

use futures::future::join_all;
use rand::RngExt;
use rand::distr::Alphanumeric;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpListener;
use tokio::sync::{Mutex, RwLock, broadcast, mpsc, watch};

const DEFAULT_ROOM_NAME: &str = "general";

#[derive(Clone)]
struct Client {
    nickname: String,
}

impl Client {
    fn new() -> Self {
        Client {
            nickname: generate_nickname(),
        }
    }
}

enum WriterMsg {
    Direct(String),
    SwitchRoom(broadcast::Receiver<(SocketAddr, String)>),
    Shutdown,
}

enum Command {
    Join(String),
    Nick(String),
    Rooms,
    Message(String),
    Invalid(String),
    Quit,
}

impl Command {
    fn parse(line: &str) -> Self {
        if !line.starts_with('/') {
            return Command::Message(line.into());
        }

        let mut parts = line.trim().split_ascii_whitespace();

        match parts.next() {
            Some("/join") => {
                if let Some(room) = parts.next() {
                    Command::Join(room.to_string())
                } else {
                    Command::Invalid("Usage: /join <room>".to_string())
                }
            }
            Some("/nick") => {
                if let Some(nickname) = parts.next() {
                    Command::Nick(nickname.to_string())
                } else {
                    Command::Invalid("Usage: /nick <nickname>".to_string())
                }
            }
            Some("/rooms") => Command::Rooms,
            Some("/quit") => Command::Quit,
            _ => Command::Invalid(format!("Unknown command message: {}", line)),
        }
    }
}

// Build a TCP chat server where:
//
//     Clients connect via TCP and join named rooms
//     Messages are broadcast to all clients in the same room
//     Commands: /join <room>, /nick <name>, /rooms, /quit
//     The server shuts down gracefully on Ctrl+C — finishing in-flight messages
#[tokio::main]
async fn main() -> io::Result<()> {
    let local_addr = "0.0.0.0:2345";
    let listener = TcpListener::bind(local_addr).await?;
    println!("Chat server listening on :2345");

    let (shutdown_tx, _) = watch::channel(false);

    let clients = Arc::new(Mutex::new(HashMap::<SocketAddr, Client>::new()));
    let rooms = Arc::new(RwLock::new(HashMap::<
        String,
        broadcast::Sender<(SocketAddr, String)>,
    >::new()));

    loop {
        tokio::select! {
            accept = listener.accept() => {
                match accept {
                    Ok((socket, remote_addr)) => {
                        println!("[{remote_addr}] Connected");
                        clients.lock().await.insert(remote_addr, Client::new());

                        let shutdown_rx = shutdown_tx.subscribe();
                        let (tx, mut rx) = mpsc::channel(100);
                        let rooms = rooms.clone();
                        let clients = clients.clone();

                        tokio::spawn(async move {
                            let (reader, mut writer) = socket.into_split();
                            let mut buf = BufReader::new(reader);
                            let mut line = String::new();

                            let default_room_tx = {
                                let mut guard_rooms = rooms.write().await;
                                get_or_create_room(&mut guard_rooms, DEFAULT_ROOM_NAME)
                            };

                            let mut tasks = Vec::new();

                            let reader_task = {
                                let mut shutdown_rx = shutdown_rx.clone();
                                let clients = clients.clone();
                                tokio::spawn(async move {
                                    let mut room_tx = default_room_tx;
                                    let _ = tx.send(WriterMsg::SwitchRoom(room_tx.subscribe())).await;

                                    loop {
                                        line.clear();
                                        tokio::select! {
                                            read_buf = buf.read_line(&mut line) => {
                                                match read_buf {
                                                    Ok(0) | Err(_) => break,
                                                    Ok(_) => {
                                                        match Command::parse(&line) {
                                                            Command::Join(room) => {
                                                                let new_room_tx = {
                                                                    let mut guard_rooms = rooms.write().await;
                                                                    get_or_create_room(&mut guard_rooms, &room)
                                                                };
                                                                room_tx = new_room_tx;
                                                                let room_rx = room_tx.subscribe();
                                                                let _ = tx.send(WriterMsg::SwitchRoom(room_rx)).await;
                                                                let _ = tx.send(WriterMsg::Direct(format!("Joined room {room}\n"))).await;
                                                            },
                                                            Command::Nick(nickname) => {
                                                                {
                                                                    let mut guard_clients = clients.lock().await;
                                                                    if let Some(client) = guard_clients.get_mut(&remote_addr) {
                                                                        client.nickname = nickname.clone();
                                                                    }
                                                                }
                                                                let _ = tx.send(WriterMsg::Direct(format!("Nickname was changed to {nickname}\n"))).await;
                                                            },
                                                            Command::Rooms => {
                                                                let room_names: Vec<String> = {
                                                                    let guard = rooms.read().await;
                                                                    guard.keys().cloned().collect()
                                                                };
                                                                let _ = tx.send(WriterMsg::Direct(format!("Rooms: {}\n", room_names.join(", ")))).await;
                                                            },
                                                            Command::Message(msg) => {
                                                                let _ = room_tx.send((remote_addr, msg));
                                                            }
                                                            Command::Invalid(msg) => {
                                                                let _ = tx.send(WriterMsg::Direct(msg)).await;
                                                            }
                                                            Command::Quit => break
                                                        }
                                                    }
                                                }
                                            }
                                            _ = shutdown_rx.changed() => {
                                                if *shutdown_rx.borrow() {
                                                    let _ = tx.send(WriterMsg::Shutdown).await;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                })
                            };
                            tasks.push(reader_task);

                            let writer_task = {
                                let clients = clients.clone();
                                tokio::spawn(async move {
                                    let mut room_rx: Option<broadcast::Receiver<(SocketAddr, String)>> = None;

                                    loop {
                                        if room_rx.is_some() {
                                            let _ = writer.write_all(">>> ".as_bytes()).await;
                                        }
                                        tokio::select! {
                                            result = rx.recv() => {
                                                match result {
                                                    Some(WriterMsg::Direct(msg)) => {
                                                        let _ = writer.write_all(msg.as_bytes()).await;
                                                    }
                                                    Some(WriterMsg::SwitchRoom(new_room_rx)) => {
                                                        room_rx = Some(new_room_rx);
                                                    }
                                                    Some(WriterMsg::Shutdown) => {
                                                        let _ = writer.write_all("Server was disconnected".as_bytes()).await;
                                                        break;
                                                    }
                                                    None => break
                                                }
                                            }
                                            result = async {
                                                room_rx.as_mut().unwrap().recv().await
                                            }, if room_rx.is_some() => {
                                                if let Ok((addr, msg)) = result {
                                                    if addr == remote_addr {
                                                        continue;
                                                    }

                                                    let nickname = {
                                                        let guard = clients.lock().await;
                                                        guard.get(&addr).map(|c| c.nickname.clone()).unwrap()
                                                    };

                                                    let _ = writer.write_all(format!("{}: {}", nickname, msg).as_bytes()).await;
                                                }
                                            }
                                        }
                                    }
                                })
                            };
                            tasks.push(writer_task);

                            join_all(tasks).await;

                            println!("[{remote_addr}] Disconnected");
                        });
                    },
                    Err(e) => println!("Error: {}", e),
                }
            }
            _ = tokio::signal::ctrl_c() => {
                println!("Shutdown signal received");
                shutdown_tx.send(true).unwrap();
                break;
            }
        }
    }

    Ok(())
}

fn generate_nickname() -> String {
    let name: String = rand::rng()
        .sample_iter(&Alphanumeric)
        .take(6)
        .map(char::from)
        .collect();
    format!("nickname-{}", name)
}

fn get_or_create_room(
    rooms: &mut HashMap<String, broadcast::Sender<(SocketAddr, String)>>,
    name: &str,
) -> broadcast::Sender<(SocketAddr, String)> {
    rooms
        .entry(name.to_string())
        .or_insert_with(|| {
            let (tx, _) = broadcast::channel(100);
            tx
        })
        .clone()
}
