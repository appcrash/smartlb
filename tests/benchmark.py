#!/usr/bin/env python3

import sys,time,random,string,hashlib
import asyncio

class Server(asyncio.Protocol):
    total_connection = 0

    def connection_made(self, transport):
        Server.total_connection += 1
        peername = transport.get_extra_info('peername')
        # print('Connection from {}'.format(peername))
        self.transport = transport

    def data_received(self, data):
        message = data.decode()
        t = Task(message)
        # print('Server Data received: {!r}'.format(message))
        self.transport.write(t.get_server_string().encode())

    def connection_lost(self,excpt):
        Server.total_connection -= 1

    async def monitor():
        while True:
            print('total connection is {}'.format(Server.total_connection))
            await asyncio.sleep(2)


class Client(asyncio.Protocol):
    def __init__(self,iteration):
        self.iteration = iteration

    def connection_made(self, transport):
        self.transport = transport
        self.task = Task()
        transport.write(self.task.get_client_string().encode())

    def data_received(self, data):
        d = data.decode()
        if not self.task.verify_server_string(d):
            print('server string not correct:{} with task:{}'.format(d,self.task))
        else:
            print('correct')
        self.iteration -= 1
        if self.iteration > 0:
            self.task = Task()
            self.transport.write(self.task.get_client_string().encode())
        else:
            self.transport.close()

        # print('Client Data received: {!r}'.format(data.decode()))

class TaskCreator():
    ''' burst rate per second, rate control
        used to create many connections
    '''
    def __init__(self,burst_rate,total_num):
        self.burst_rate = burst_rate
        self.total_num = total_num
        self.q = asyncio.Queue()

    async def fire(self):
        index = 0
        while self.total_num > 0:
            timestamp1 = time.time_ns()
            for i in range(self.burst_rate):
                self.q.put_nowait(index)  # just id
                index += 1
            timestamp2 = time.time_ns()

            sleep_for = 1 - (timestamp2 - timestamp1) / 1_000_000_000
            if sleep_for > 0:
                await asyncio.sleep(sleep_for)
            self.total_num -= self.burst_rate

    async def do_task(self,task_factory):
        while True:
            i = await self.q.get()
            print(i)
            asyncio.create_task(task_factory())
            self.q.task_done()

class Task():
    ''' client line format: CMD LEN DATA
        server line format: LEN DATA
    '''
    TASK_TYPE = ['echo','md5']
    ALL_CHARACTER = string.ascii_letters + string.digits
    class NeedMoreException(Exception): pass

    def __init__(self,client_str_line : str = None):
        if client_str_line is not None:
            self._parse(client_str_line)

    def __str__(self):
        return 'type: {}, data:{}'.format(self.task_type,self.data)

    def _parse(self,str_line : str):
        sp = str_line.find(' ')
        if sp == -1:
            raise Exception('{} has no whitespace'.format(str_line))
        else:
            self.task_type = str_line[:sp]

        sp2 = str_line.find(' ',sp + 1)
        if sp2 == -1:
            raise Exception('{} has no second whitespace'.format(str_line))
        else:
            length = int(str_line[sp + 1 : sp2])

        if (sp2 + 1 + length) != len(str_line):
            raise NeedMoreException('length not correct: sp1:{},sp2:{},length:{} str_length:{}'.format(sp,sp2,length,len(str_line)))
        self.data = str_line[sp2 + 1:]

    def _random_string(self,length):
        return ''.join(random.choice(Task.ALL_CHARACTER) for i in range(length))

    def get_client_string(self):
        self.task_type = random.choice(Task.TASK_TYPE)
        self.data = self._random_string(20)
        return '{} {} {}'.format(self.task_type,len(self.data),self.data)

    def get_server_string(self):
        if self.task_type == 'echo':
            resp = self.data
        elif self.task_type == 'md5':
            resp = hashlib.md5(self.data.encode()).hexdigest()
        else:
            resp = 'error'

        return '{} {}'.format(len(resp),resp)

    def verify_server_string(self,ss):
        return self.get_server_string() == ss


def usage():
    print('''
        1) benchmark server ip port
        2) benchmark client ip port burst_rate total_number client_iteration
    ''')



async def main():
    cmd = sys.argv[1]
    loop = asyncio.get_running_loop()

    if cmd == 'server':
        (ip,port) = sys.argv[2:]
        server = await loop.create_server(
            lambda: Server(),ip,port,backlog=10000)
        asyncio.create_task(Server.monitor())
        async with server:
            await server.serve_forever()
    elif cmd == 'client':
        (ip,port,burst,total,iteration) = sys.argv[2:]
        (burst,total,iteration) = map(lambda s: int(s),(burst,total,iteration))
        creator = TaskCreator(burst,total)

        task_factory = lambda: loop.create_connection(
            lambda: Client(iteration),ip,port)
        asyncio.create_task(creator.do_task(task_factory))
        await creator.fire()
    else:
        usage()

asyncio.run(main())