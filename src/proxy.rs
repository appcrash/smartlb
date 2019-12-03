use tokio::prelude::*;
use std::future::Future;
use std::io;
use std::pin::Pin;
use std::task::{Context,Poll};


pub const LISTEN_ADDRESS : &str = "127.0.0.1:7000";
pub const UPSTREAM_ADDRESS : &str = "127.0.0.1:9000"; 


pub struct Proxy<'a,R: AsyncRead,W: AsyncWrite> 
{
    left_reader : &'a mut R,
    left_writer : &'a mut W,

    right_reader : Option<R>,
    right_writer : Option<W>,
    left_receive_buf : Box<[u8]>,
    right_receive_buf : Box<[u8]>,
}


impl<'a,R,W> Proxy<'a,R,W>
where
  R: AsyncRead + Unpin,
  W : AsyncWrite + Unpin
{
    pub fn new(reader : &'a mut R, writer : &'a mut W) -> Proxy<'a,R,W> {
        Proxy{
            left_reader: reader,
            left_writer: writer,
            right_reader: None,
            right_writer: None,
            left_receive_buf : Box::new([0;1024]),
            right_receive_buf : Box::new([0;1024]),
        }
    }

    pub async fn run(&mut self) {
        self.left_reader.read(&mut self.left_receive_buf);
        println!("data is {:?}",self.left_receive_buf);
    }
}

// impl<'a,R,W> Future for Proxy<'a,R,W>
// where R: AsyncRead + Unpin,
// W : AsyncWrite + Unpin
// {
//     type Output = io::Result<u64>;


//     fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<io::Result<u64>> {
//         self.left_reader.read(&mut self.left_receive_buf);
//         println!("data is {:?}",self.left_receive_buf);
//         Poll::Ready(Ok(0))
//     }
// }
