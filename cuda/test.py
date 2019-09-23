import csv
import re
from os import path

avg_col = 7
metric_index = 3
#all_bench = ['backprop', 'bfs', 'b+tree', 'cfd', 'dwt2d', 'gaussian', 'heartwall', 'hotspot', 'hotspot3D', 'huffman', 'hybridsort', 'kmeans', 'lavaMD', 'leukocyte', 'lud', 'mummergpu', 'myocyte', 'nn', 'nw', 'particlefilter', 'pathfinder', 'srad_v1', 'srad_v2', 'streamcluster']
all_bench = ['backprop', 'bfs', 'b+tree', 'cfd', 'dwt2d', 'gaussian', 'heartwall', 'hotspot', 'hotspot3D', 'huffman', 'hybridsort', 'kmeans', 'lavaMD', 'leukocyte', 'lud', 'myocyte', 'nn', 'nw', 'particlefilter', 'pathfinder', 'srad_v1', 'srad_v2', 'streamcluster']

metrics_path = '/home/edwardhu/rodinia/cuda/'
#metrics_path = '/home/ed/Desktop/Rodinia/cuda/'
metrics_filename = '/metrics.csv'

def open_file(filename, benchmark):
    dict_to_return = {"benchmark name": benchmark}
    # check whether the file exists
    if not path.exists(filename):
        print('no such file')
        exit(1)

    with open(filename) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')
        line_count = 0
        for row in csv_reader:
            if line_count < 5:
                pass
            elif line_count == 5:
                print(row)
            else:
                key = row[metric_index]
                avg = row[avg_col]

                if avg.endswith("%"):
                    avg_val = re.findall('\d*\.?\d+', avg)
                    if (len(avg_val) > 1):
                        print("wrong parsing 1")
                        exit(1)
                    avg_val[0] = float(avg_val[0]) / 100
                else:
                    avg_val = re.findall('\d*\.?\d+', avg)
                    if (len(avg_val) > 1):
                        print("wrong parsing 2")
                        exit(1)

                # print(avg_val[0])
                
                if key not in dict_to_return:
                    dict_to_return[key] = avg_val[0]
                else:
                    tmp = dict_to_return[key]
                    if float(tmp) < float(avg_val[0]):
                        dict_to_return[key] = avg_val[0]
    
            line_count += 1
    
    return dict_to_return

def main():
    with open('all_metrics.csv', mode='w') as csv_file:
        for i, bench in enumerate(all_bench):
            full_path = metrics_path + bench + metrics_filename
            result = open_file(full_path, bench)
            if i == 0:
                fieldnames = result.keys()
                writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
                writer.writeheader()
                writer.writerow(result)
            else:
                writer.writerow(result)

            # for key in result:
            #     print(key, "    ", result[key])


if __name__ == "__main__":
    main()
