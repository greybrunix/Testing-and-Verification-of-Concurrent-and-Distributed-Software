import threading

N = 100000000
count = 0
done = [False, False]

def incr(self):
    global count
    for i in range(N):
        count = count+1
    done[self] = True
    while not done[1 - self]:
        pass
    assert count == 2*N, count

threading.Thread(target=incr, args=(0,)).start()
threading.Thread(target=incr, args=(1,)).start()
