import csv
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('folder_name')
args = parser.parse_args()

if args.folder_name == "comp_tools":
    data_lst = { "res1-polka-standard": [], "res_rtype": [], 
        "res_dorder": [], "res_dsolve": [], "res_mochi": []}
    res_lst = { "res1-polka-standard": [], "res_rtype": [], 
        "res_dorder": [], "res_dsolve": [], "res_mochi": []}
    csv_lst = ["res1-polka-standard", "res_rtype", "res_dorder", "res_dsolve", "res_mochi"]
else:
    data_lst = {"res-oct-standard": [], "res-oct-wid+nar": [], 
        "res-oct-dwid-300": [], "res-polka-standard": [],
        "res-polka-wid+nar": [], "res-polka-dwid-300": [], "res-ppl_st-standard": [], 
        "res-ppl_st-wid+nar": [], "res-ppl_st-dwid-300": [], "res1-oct-standard": [], "res1-oct-wid+nar": [], 
        "res1-oct-dwid-300": [], "res1-polka-standard": [],
        "res1-polka-wid+nar": [], "res1-polka-dwid-300": [], "res1-ppl_st-standard": [], 
        "res1-ppl_st-wid+nar": [], "res1-ppl_st-dwid-300": []
    }
    res_lst = {"res-oct-standard": [], "res-oct-wid+nar": [], 
        "res-oct-dwid-300": [], "res-polka-standard": [],
        "res-polka-wid+nar": [], "res-polka-dwid-300": [], "res-ppl_st-standard": [], 
        "res-ppl_st-wid+nar": [], "res-ppl_st-dwid-300": [], "res1-oct-standard": [], "res1-oct-wid+nar": [], 
        "res1-oct-dwid-300": [], "res1-polka-standard": [],
        "res1-polka-wid+nar": [], "res1-polka-dwid-300": [], "res1-ppl_st-standard": [], 
        "res1-ppl_st-wid+nar": [], "res1-ppl_st-dwid-300": []
    }
    csv_lst = ["res-oct-standard", "res-oct-wid+nar", "res-oct-dwid-300", "res-polka-standard",
"res-polka-wid+nar", "res-polka-dwid-300", "res-ppl_st-standard", "res-ppl_st-wid+nar", "res-ppl_st-dwid-300",
"res1-oct-standard", "res1-oct-wid+nar", "res1-oct-dwid-300", "res1-polka-standard",
"res1-polka-wid+nar", "res1-polka-dwid-300", "res1-ppl_st-standard", "res1-ppl_st-wid+nar", "res1-ppl_st-dwid-300" ]

sort_lst = ["high", "first", "array", "negative"]
unit_lst = ["succ", "total", "avg.", "mean"]

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
    if args.folder_name == "comp_tools":
        for filep in csv_lst:
            if file_name != filep:
                for dicp in data_lst[filep]:
                    if bench == dicp['bench'] and is_only:
                        if dicp['data'][idx][2] == 'T':
                            return 0
            else: continue
        return 1
    else:
        name = file_name.split('-')[1] # get domain
        tool_ver = file_name.split('-')[0]
        for filep in csv_lst:
            comp_name = file_name.split('-')[1] # get comp domain
            comp_tool_ver = file_name.split('-')[0]
            if file_name != filep and name == comp_name and tool_ver == comp_tool_ver:
                for dicp in data_lst[filep]:
                    if bench == dicp['bench'] and is_only:
                        if dicp['data'][idx][2] == 'T':
                            return 0
            else: continue
        return 1

def cal_res():
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

def print_for_table2():
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

def print_for_table1():
    for bench in sort_lst:
        for row in range(4):
            print("\t\t", end = '&')
            print(" "+unit_lst[row], end = '\t')
            for csv in csv_lst:
                for dic in res_lst[csv]:
                    if dic['bench'] == bench:
                        d = dic["data"][row]
                        if row == 0:
                            dp = dic["data"][4]
                            if dp == 0: print(f'& {d}', end = '\t')
                            else: print(f'& {d}({dp})', end = '\t')
                        elif isinstance(d, float):
                            print(f'& {d:.2f}', end = '\t')
                        else: print(f'& {d}', end = '\t')
            print("\\\\")
        print("\hline")

def main():
    read_data()
    cal_res()
    if args.folder_name == "comp_tools":
        print_for_table2()
    else:
        print_for_table1()

if __name__ == '__main__':
    main()