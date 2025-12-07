with open("inputs/input_6", "r") as f:
    lines = f.readlines()
    ops = lines[-1]
    numbers = lines[:-1:]
    
    splits = [i-1 for (i, o) in enumerate(ops) if o != ' ' and i > 0]
    operations = [o for o in ops if o != ' ']

    cols = []
    currs = []
    for i in range(len(numbers[0])):
        if i in splits:
            cols.append(currs)
            currs = []
        else:
            curr = 0
            for j in range(len(numbers)):
                c = numbers[j][i]
                if c != ' ' and c != '\n':
                    curr = (curr * 10) + int(c)
            currs.append(curr)
    cols.append(currs)

    max_len = max(len(c) for c in cols)
    for (i, c) in enumerate(cols):
        while len(c) < max_len:
            if operations[i] == "+":
                c.append(0)
            else:
                c.append(1)

    res = ""
    for i in range(len(cols[0])):
        curr = ""
        for j in range(len(cols)):
            curr += "" + str(cols[j][i]) + " "
        res = res + curr + '\n'
        
    with open("inputs/input_6_preprocessed", "w") as w:
        w.write(res)