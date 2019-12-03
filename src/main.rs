use std::io;
use tokio::net::TcpListener;
use smart_lb::proxy::Proxy;
use tokio::prelude::*;
use futures;


#[tokio::main]
async fn main() -> io::Result<()> {
    // let addr = "127.0.0.1:7000".parse()?;
    // let upstream_addr = "127.0.0.1:9000".parse()?;
    let mut listener = TcpListener::bind(smart_lb::proxy::LISTEN_ADDRESS).await?;

    loop {
        let (mut stream, addr) = listener.accept().await?;
        println!("ip is {:?}",addr.ip());

        tokio::spawn(async move {
            let (mut reader,mut writer) = stream.split();
            let mut p = Proxy::new(&mut reader,&mut writer);
            let re = p.run().await;
            println!("result is {:?}",re);
        });
        io::Result::Ok(());
    }
}
