use tokio::{net::TcpListener, io::{AsyncReadExt, AsyncWriteExt}};

#[tokio::main]
async fn main() {
    let listener = TcpListener::bind("localhost:8181").await.unwrap();
    let (mut socket, _addr) = listener.accept().await.unwrap();

    loop {
        let mut buf = [0u8; 1024];
        let buf_size = socket.read(&mut buf).await.unwrap();
        socket.write_all(&buf[..buf_size]).await.unwrap();
    }
}
