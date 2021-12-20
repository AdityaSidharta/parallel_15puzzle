import random


if __name__ == '__main__':
    
    case_num = 100
    outfile = "./test5.txt"

    with open(outfile, 'w') as f:
        f.write(f"{case_num}\n")
        for i in range(case_num):
            size = 4 # random.randint(2,4)
            f.write(f"{size}\n")
            l = [i for i in range(size*size)]
            random.shuffle(l)
            for i in range(len(l)):
                if (i+1) % size == 0:
                    f.write(f"{l[i]}\n")
                else:
                    f.write(f"{l[i]} ")
    
