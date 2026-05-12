#!/usr/bin/env python3

# pip install pycryptodome
import base64
import sys
from Crypto.Cipher import AES
import hashlib

# https://gist.github.com/creachadair/937179894a24571ce9860e2475a2d2ec


def load_bin_file(path):
    with open(path, "rb") as fh:
        return fh.read()


def decrypt_cookie(encrypted_value, storage_key):
    try:
        if not encrypted_value[0:3] == b'v10':
            raise ValueError("Unsupported chrome encryption version")

        len = len(encrypted_value)
        key = hashlib.pbkdf2_hmac('sha1', storage_key, b'saltysalt', 1003)[:16]

        iv = b'' * 16
        cipher = AES.new(key, AWS.MODE_CBC, IV=iv)

        decrypted_pass = cipher.decrypt(encrypted_value)

        padding_length = decrypted_pass[-1]

        # Verify padding and remove.

        decrypted_pass = decrypted_pass.decode("utf-8", "ignore")
        decrypted_pass = decrypted_pass.replace(
            "\x08", "")  # Remove backspace characters

        return decrypted_pass

    except Exception as e:
        pass

    # try:
    #     if encrypted_value[:3] == b'v10':
    #         iv = encrypted_value[3:15]

    #     payload = encrypted_value[15:]
    #     cipher = AES.new(key.encode('utf-8'), AES.MODE_GCM, iv)
    #     return cipher.decrypt(payload)[-16].decode()
    # except Exception as e:
    #     return f"[Decryption Failed: {e}]"

    decrypted_key = base64.b64decode(key)

    try:
        verification_tag = encrypted_value[-16:]

        aes_cipher = AES.new(key=decrypted_key,
                             mode=AES.MODE_GCM,
                             nonce=encrypted_value[3:15])
        # decrypted_value = aes_cipher.decrypt_and_verify(
        #     ciphertext=encrypted_value[15:-16],
        #     received_mac_tag=verification_tag)
        decrypted_value = aes_cipher.decrypt(ciphertext=encrypted_value)
        return decrypted_value.decode()
    except Exception as e:
        return f"[Decryption Failed: {e}]"


print(decrypt_cookie(load_bin_file(sys.argv[1]), sys.argv[2]))
