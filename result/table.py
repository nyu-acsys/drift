import csv
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('folder_name')
args = parser.parse_args()

if args.folder_name == "comp_tools":
    data_lst = { "res-polka-standard": [], "res_rtype": [], 
        "res_dorder": [], "res_dsolve": [], "res_mochi": []}
    res_lst = { "res-polka-standard": [], "res_rtype": [], 
        "res_dorder": [], "res_dsolve": [], "res_mochi": []}
    csv_lst = ["res-polka-standard", "res_rtype", "res_dorder", "res_dsolve", "res_mochi"]
else:
    data_lst = {"res-oct-standard": [], "res-oct-wid+nar": [], 
        "res-oct-dwid-300": [], "res-polka-standard": [],
        "res-polka-wid+nar": [], "res-polka-dwid-300": [], "res-ppl_st-standard": [], 
        "res-ppl_st-wid+nar": [], "res-ppl_st-dwid-300": []
    }
    res_lst = {"res-oct-standard": [], "res-oct-wid+nar": [], 
        "res-oct-dwid-300": [], "res-polka-standard": [],
        "res-polka-wid+nar": [], "res-polka-dwid-300": [], "res-ppl_st-standard": [], 
        "res-ppl_st-wid+nar": [], "res-ppl_st-dwid-300": []
    }
    csv_lst = ["res-oct-standard", "res-oct-wid+nar", "res-oct-dwid-300", "res-polka-standard",
"res-polka-wid+nar", "res-polka-dwid-300", "res-ppl_st-standard", "res-ppl_st-wid+nar", "res-ppl_st-dwid-300" ]

sort_lst = ["high", "first", "array", "negative"]

def read_data():
    for file_name in csv_lst:
        with open(args.folder_name+"/"+file_name+".csv") as csv_file:
            csv_reader = csv.reader(csv_file, delimiter=',')
            set_name = ""
            data_set = {"bench": set_name,  "data": []}
            line_count = 0
            d_lst = []
            for row in csv_reader:
                if line_count == 0:
                    line_count += 1
                    continue
                elif set_name != row[0]:
                    if set_name != "":
                        set_name = row[0]
                        d_lst.append(data_set)
                        data_set = {"bench": set_name,  "data": []}
                    else: 
                        set_name = row[0]
                        data_set["bench"] = set_name
                try:
                    time = float(row[3])
                except:
                    time = row[3]
                data = [row[0], time, row[4]]
                data_set["data"].append(data)
            d_lst.append(data_set)
            data_lst[file_name] = d_lst
            new_lst = []
            for bn in sort_lst:
                for dic in data_lst[file_name]:
                    if dic['bench'] == bn:
                        new_lst.append(dic)
                    else: continue
            data_lst[file_name] = new_lst

def cal_solve(d, file_name, bench, idx):
    is_only = True
    for filep in csv_lst:
        if file_name != filep:
            for dicp in data_lst[filep]:
                if bench == dicp['bench'] and is_only:
                    if dicp['data'][idx][2] == 'T':
                        return 0
        else: continue
    return 1

def print_res():
    for file in csv_lst:
        for dic in data_lst[file]:
            full_time = 0.0
            avg_time = 0.0
            mean_time = 0.0
            total_count = 0
            succ_count = 0
            max_time = 0.0
            min_time = 600.0
            solve_by_this = 0
            for i in range(0, len(dic["data"])):
                d = dic["data"][i]
                if isinstance(d[1], float):
                    if d[2] == 'T':
                        solve_by_this += cal_solve(d, file, dic['bench'], i)
                        succ_count += 1
                    if d[1] > max_time:
                        max_time = d[1]
                    if d[1] < min_time:
                        min_time = d[1]
                    full_time += d[1]
                total_count += 1
            mean_time = round((max_time + min_time) / 2, 2)
            full_time = round(full_time, 2)
            avg_time = round(full_time / total_count, 2)
            bench_name = dic['bench']
            new_dic = {'bench': "", "data": []}
            new_dic['bench'] = bench_name
            new_dic["data"] = [succ_count, full_time, avg_time, mean_time, solve_by_this]
            res_lst[file].append(new_dic)

def print_by_table():
    for csv in csv_lst:
        print(f'======={csv}========')
        for dic in res_lst[csv]:
            bench_name = dic['bench']
            succ_count = dic["data"][0]
            full_time = dic["data"][1]
            avg_time = dic["data"][2]
            mean_time = dic["data"][3]
            solve_by_this = dic["data"][4]
            print("bench | succ | full | avg | mean | solvethis")
            print(f'{bench_name} | {succ_count} | {full_time:.2f} | {avg_time:.2f} | {mean_time:.2f} | {solve_by_this}')
            print("\n")
        print(f'=======end========')

def main():
    read_data()
    print_res()
    print_by_table()

if __name__ == '__main__':
    main()