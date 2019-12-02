use std::io;
use tokio::net::{TcpStream,TcpListener};
use tokio::prelude::*;


#[tokio::main]
async fn main() -> io::Result<()> {
    // let addr = "127.0.0.1:7000".parse()?;
    // let upstream_addr = "127.0.0.1:9000".parse()?;
    let mut listener = TcpListener::bind(smart_lb::proxy::LISTEN_ADDRESS).await?;

    loop {
        let (_, addr) = listener.accept().await?;
        println!("ip is {:?}",addr.ip());
        io::Result::Ok(());
    }
}
