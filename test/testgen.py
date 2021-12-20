import random
import numpy as np

dirs = [-1,0,1,0,-1]

def swapzero(step, n):
    arr = np.array([i for i in range(n*n)])
    x , y = 0, 0
    for _ in range(step):
        d = random.randint(0,3)
        dx = x + dirs[d]
        dy = y + dirs[d+1]
        if dx >= 0 and dy >= 0 and dx < n and dy < n:
            tmp = arr[x*n+y]
            arr[x*n+y] = arr[dx*n+dy]
            arr[dx*n+dy] = tmp
            x = dx
            y = dy
    return arr


if __name__ == '__main__':
    
    case_num = 100
    outfile = "./test5.txt"

    with open(outfile, 'w') as f:
        f.write(f"{case_num}\n")
        for i in range(case_num):
            size = 4 # random.randint(2,4)
            f.write(f"{size}\n")
            l = swapzero(120, size)
            for i in range(l.shape[0]):
                if (i+1) % size == 0:
                    f.write(f"{l[i]}\n")
                else:
                    f.write(f"{l[i]} ")
    
