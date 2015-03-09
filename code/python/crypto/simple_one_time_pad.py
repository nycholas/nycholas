#!/usr/bin/env python
# -* coding: utf-8 -*-
A, Z = 65, 90
M = (Z - A) + 1

def otp_encrypt(key, plaintext):
    """http://en.wikipedia.org/wiki/One-time_pad

    >>> otp_encrypt('XMCKL', 'HELLO')
    'EQNVZ'
    """
    key = key.upper()
    plaintext = plaintext.upper()
    ciphertext = []
    for c, k in zip(list(plaintext), list(key)):
        ciphertext.append(((ord(c) - A) + (ord(k) - A)) % M)
    return ''.join([chr(c + A) for c in ciphertext])

def otp_decrypt(key, ciphertext):
    """http://en.wikipedia.org/wiki/One-time_pad

    >>> otp_decrypt('XMCKL', 'EQNVZ')
    'HELLO'
    """
    key = key.upper()
    ciphertext = ciphertext.upper()
    plaintext = []
    for c, k in zip(list(ciphertext), list(key)):
        plaintext.append(((ord(c) - A) - (ord(k) - A)) % M)
    return ''.join([chr(c + A) for c in plaintext])

if __name__ == '__main__':
    import doctest
    doctest.testmod()
