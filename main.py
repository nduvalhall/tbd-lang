def factorial(n):
    def aux(n, acc):
        print(n, acc)
        if n == 0:
            return acc
        return aux(n - 1, n * acc)
    return aux(n, 1)


def fibonacci(n):
    def aux(i, prev, next):
        if i == n:
            return next
        return aux(i + 1, next, next + prev)
    return aux(1, 0, 1)




print(fibonacci(994))
