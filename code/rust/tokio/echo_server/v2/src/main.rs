use tokio::{
    io::{AsyncReadExt, AsyncWriteExt, BufReader, AsyncBufReadExt},
    net::TcpListener,
};

#[tokio::main]
async fn main() {
    let listener = TcpListener::bind("localhost:8181").await.unwrap();

    loop {
        let (mut socket, _addr) = listener.accept().await.unwrap();

        tokio::spawn(async move {
            let (reader, mut writer) = socket.split();
            let mut buf_reader = BufReader::new(reader);

            loop {
                let mut line = String::new();
                let bytes_size = buf_reader.read_line(&mut line).await.unwrap();
                if bytes_size == 0 {
                    break;
                }

                writer.write_all(line.as_bytes()).await.unwrap();
            }
        });
    }
}
