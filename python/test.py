from PyHVM import execute_as_hvm
from inspect import getsource

def hvm(f):
    def wrapper(*args, **kwargs):
        return execute_as_hvm(f.__name__, getsource(f))
    return wrapper

@hvm
def main():
    return funcao()

print(main(3))
