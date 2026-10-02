import sys
import matplotlib.pyplot as plt

all_traces = []
current_trace = None

for line in sys.stdin:
    line = line.strip()
    if not line:
        continue

    if line.startswith("!"):
        print(line.lstrip("!"))
        continue

    if line == "begin trace":
        current_trace = { "x": [], "y": [], "kwargs": {} }
        
    elif line == "end trace":
        if current_trace is not None:
            all_traces.append(current_trace)
            current_trace = None
            
    elif current_trace is not None:

        if line.startswith("point:"):
            coords = line.split(":", 1)[1].strip().replace(",", " ").split()
            current_trace["x"].append(float(coords[0]))
            current_trace["y"].append(float(coords[1]))
        else:
            parts = line.split(":", 1)
            current_trace["kwargs"][parts[0].strip()] = parts[1].strip()


for trace in all_traces:
    plt.plot(trace["x"], trace["y"], **trace["kwargs"])

plt.legend()
plt.show()
