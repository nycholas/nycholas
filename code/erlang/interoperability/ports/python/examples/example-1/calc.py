#!/usr/bin/env python3
import os
import struct

FD_STDIN = 0
FD_STDOUT = 1


def sum(a, b):
    return a + b


def subtract(a, b):
    return a - b


def multiplies(a, b):
    return a * b


def divides(a, b):
    return a // b


def main():
    try:
        while buffer := read():
            fn, a, b = decode(buffer)
            if fn == 1:
                result = sum(a, b)
                message_data = encode(result)
                write(message_data)
            elif fn == 2:
                result = subtract(a, b)
                message_data = encode(result)
                write(message_data)
            elif fn == 3:
                result = multiplies(a, b)
                message_data = encode(result)
                write(message_data)
            elif fn == 4:
                result = divides(a, b)
                message_data = encode(result)
                write(message_data)
            else:
                raise ValueError()
    finally:
        close()


def encode(message):
    return struct.pack('!H', message)


def decode(buffer):
    return struct.unpack('!BBB', buffer)


def read(packet=2, buffer_size=65536):
    buffer = b''
    while len(buffer) < packet:
        buffer += os.read(FD_STDIN, buffer_size)
    length = struct.unpack('!H', buffer[:packet])[0] + packet
    while len(buffer) < length:
        buffer += os.read(FD_STDIN, buffer_size)
    return buffer[packet:]


def write(data, packet=2):
    length = len(data)
    data = struct.pack('!H', length) + data
    while data:
        n = os.write(FD_STDOUT, data)
        data = data[n:]
    return length + packet


def close():
    os.close(FD_STDIN)
    os.close(FD_STDOUT)


if __name__ == "__main__":
    main()