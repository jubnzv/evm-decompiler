#!/usr/bin/python3
import os
import subprocess


def compile_all(path: str):
    for f in os.listdir(path):
        f = os.path.join(path, f)
        if not os.path.isfile(f) or not f.endswith(".sol"):
            continue
        p = subprocess.Popen(["solc", "--bin", f],
                             stdout=subprocess.PIPE)
        p.wait()
        if p.returncode != 0:
            print(f"{f}: solc return code {p.returncode}")
            continue
        if not p.stdout:
            print(f"{f}: solc doesn't produce bytecode")
            continue
        bytecode = p.stdout.read().decode().split('\n')[3]
        with open(f"{f[:-3]}bin", "w") as out_file:
            out_file.write(bytecode)


if __name__ == '__main__':
    compile_all('.')
