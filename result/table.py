import csv
import argparse
import re
import copy

exp2_regexp = re.compile(r'comp_tools')
parser = argparse.ArgumentParser()
parser.add_argument('folder_name')
parser.add_argument('show_unsolved', nargs='?', default=False)
args = parser.parse_args()

if exp2_regexp.search(args.folder_name):
    data_lst = { "res1-polka_ls-thowid": [], "res-rtype-z3": [], 
         "res-dsolve": [], "res-mochi-hoice": []}
    res_lst = { "res1-polka_ls-thowid": [], "res-rtype-z3": [], 
         "res-dsolve": [], "res-mochi-hoice": []}
    csv_lst = ["res1-polka_ls-thowid", "res-rtype-z3", "res-dsolve", "res-mochi-hoice"]
else:
    data_lst = {"res-oct-standard": [], 
        "res-oct-thowid": [], "res-polka_st-standard": [], "res-polka_st-thowid": [], "res-polka_ls-standard": [], 
         "res-polka_ls-thowid": [], "res1-oct-standard": [], 
        "res1-oct-thowid": [], "res1-polka_st-standard": [],
         "res1-polka_st-thowid": [], "res1-polka_ls-standard": [], 
         "res1-polka_ls-thowid": []
    }
    res_lst = {"res-oct-standard": [], 
        "res-oct-thowid": [], "res-polka_st-standard": [], "res-polka_st-thowid": [], "res-polka_ls-standard": [], 
         "res-polka_ls-thowid": [], "res1-oct-standard": [], 
        "res1-oct-thowid": [], "res1-polka_st-standard": [],
         "res1-polka_st-thowid": [], "res1-polka_ls-standard": [], 
         "res1-polka_ls-thowid": []
    }
    csv_lst = ["res-oct-standard", "res-oct-thowid", "res-polka_st-standard",
 "res-polka_st-thowid", "res-polka_ls-standard", "res-polka_ls-thowid",
"res1-oct-standard", "res1-oct-thowid", "res1-polka_st-standard",
"res1-polka_st-thowid", "res1-polka_ls-standard", "res1-polka_ls-thowid" ]

# loc is calculated by cloc 
sort_lst = {"first":["FO", 11], "high":["HO", 10], "termination":["T", 44], "array":["A", 17], "list":["L", 16], "negative":["E", 21]}
unit_lst = ["succ", "total", "avg.", "mean"]

cant_solve_lst = [
    { "oct":[], "polka_st":[], "polka_ls":[] },
    { "oct":[], "polka_st":[], "polka_ls":[] }
]

can_solve_domain_dic = { 
"oct":[{"first":0, "high":0, "termination":0, "array":0, "list":0, "negative":0},{"first":0, "high":0, "termination":0, "array":0, "list":0, "negative":0}], 
"polka_st":[{"first":0, "high":0, "termination":0, "array":0, "list":0, "negative":0},{"first":0, "high":0, "termination":0, "array":0, "list":0, "negative":0}],
"polka_ls":[{"first":0, "high":0, "termination":0, "array":0, "list":0, "negative":0},{"first":0, "high":0, "termination":0, "array":0, "list":0, "negative":0}]
}

def get_domain_idx(domain):
    if domain == "oct": return 0
    elif domain == "polka_st": return 1
    else: return 2

def get_bench_idx(bench):
    if bench == "high": return 0
    elif bench == "first": return 1
    elif bench == "array": return 2
    elif bench == "list": return 3
    elif bench == "termination": return 4
    else: return 5

def read_data():
    for file_name in csv_lst:
        if exp2_regexp.search(args.folder_name):
            csv_file = open(args.folder_name+"/"+file_name+".csv", 'r')
        else:
            try:
                csv_file = open("non-sensitive/"+args.folder_name+"/"+file_name+".csv", 'r')
            except:
                csv_file = open("1-sensitive/"+args.folder_name+"/"+file_name+".csv", 'r')
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
            data = [row[0], time, row[4], row[1]]
            data_set["data"].append(data)
        d_lst.append(data_set)
        data_lst[file_name] = d_lst
        new_lst = []
        for bn, _ in sort_lst.items():
            for dic in data_lst[file_name]:
                if dic['bench'] == bn:
                    new_lst.append(dic)
                else: continue
        data_lst[file_name] = new_lst


def cal_solve(d, file_name, bench, idx):
    if exp2_regexp.search(args.folder_name):
        for filep in csv_lst:
            if file_name != filep:
                for dicp in data_lst[filep]:
                    if bench == dicp['bench']:
                        if dicp['data'][idx][2] == 'T':
                            return 0
            else: continue
        return 1
    else:
        tool_ver = file_name.split('-')[0]
        for filep in csv_lst:
            comp_tool_ver = filep.split('-')[0]
            if file_name != filep and tool_ver == comp_tool_ver:
                for dicp in data_lst[filep]:
                    if bench == dicp['bench']:
                        if dicp['data'][idx][2] == 'T':
                            return 0
            else: continue
        return 1

def common_not_solved(file_name, bench, idx, test_name):
    domain_name = file_name.split('-')[1] # get domain
    tool_ver = file_name.split('-')[0]
    common = True
    for filep in csv_lst:
        comp_domain_name = filep.split('-')[1] # get comp domain
        comp_tool_ver = filep.split('-')[0]
        if file_name != filep  and tool_ver == comp_tool_ver: # and domain_name == comp_domain_name
            for dicp in data_lst[filep]:
                # if bench == dicp['bench'] and test_name == "mc91_95":
                #     print(test_name, filep, dicp['data'][idx][2])
                if common and bench == dicp['bench']:
                    if dicp['data'][idx][2] == 'F':
                        continue
                    else: common = False
        else: continue
    test_name = bench + "/" + test_name
    idx = 0 if tool_ver == "res" else 1
    if common and test_name not in cant_solve_lst[idx][domain_name]:
        cant_solve_lst[idx][domain_name].append(test_name)

def cal_res():
    idx = 0
    total_time_1 = 0.0
    total_time_2 = 0.0
    for file in csv_lst:
        for dic in data_lst[file]:
            full_time = 0.0
            avg_time = 0.0
            mean_time = 0.0
            total_count = 0
            succ_count = 0
            median_time = 0.0
            max_time = 0.0
            min_time = 600.0
            solve_by_this = 0
            timeout_count = 0
            sort_timing_lst = []
            for i in range(0, len(dic["data"])):
                d = dic["data"][i]
                if isinstance(d[1], float):
                    sort_timing_lst.append(d[1])
                    if d[2] == 'T':
                        solve_by_this += cal_solve(d, file, dic['bench'], i)
                        succ_count += 1
                    elif not exp2_regexp.search(args.folder_name):
                        common_not_solved(file, dic['bench'], i, d[3])
                    if d[1] > max_time:
                        max_time = d[1]
                    if d[1] < min_time:
                        min_time = d[1]
                    full_time += d[1]
                else: timeout_count += 1
                total_count += 1
            sorted(sort_timing_lst)
            # 0,1,2,3
            l = len(sort_timing_lst)
            median_time = sort_timing_lst[(l - 1) // 2] if l % 2 != 0 \
                else round((sort_timing_lst[l // 2] + sort_timing_lst[l // 2 - 1]) / 2 , 2)
            mean_time = round((max_time + min_time) / 2, 2)
            full_time = round(full_time, 2)
            avg_time = round(full_time / total_count, 2)
            bench_name = dic['bench']
            new_dic = {'bench': "", "data": []}
            new_dic['bench'] = bench_name
            new_dic["data"] = [succ_count, full_time, median_time, avg_time, mean_time, solve_by_this, total_count, timeout_count]
            res_lst[file].append(new_dic)
            if idx < 6: total_time_1 += full_time
            else: total_time_2 += full_time
        idx += 1
    # print(total_time_2 / total_time_1)

def print_for_table2():
    for csv in csv_lst:
        print(f'======={csv}========')
        for dic in res_lst[csv]:
            bench_name = dic['bench']
            succ_count = dic["data"][0]
            full_time = dic["data"][1]
            median_time = dic["data"][2]
            avg_time = dic["data"][3]
            mean_time = dic["data"][4]
            solve_by_this = dic["data"][5]
            timeout_count = dic["data"][7]
            print("bench | succ(solvethis) | full(timeout) | average | median | ")
            print(f'{bench_name}\t',end='&')
            if solve_by_this == 0:
                print(f' {succ_count}\t',end='&')
            else:
                print(f' {succ_count}({solve_by_this})\t',end='&')
            if timeout_count == 0:
                print(f' {full_time:.2f}\t',end='&')
            else:
                print(f' {full_time:.2f}({timeout_count})\t',end='&')
            print(f' {avg_time:.2f}\t', end='&')
            print(f' {median_time:.2f}\t')
            # print(f' {mean_time:.2f}\t')
            print("\n")
        print(f'=======end========')

def print_for_table1():
    for bench, conf in sort_lst.items():
        for row in range(2):
            if row == 0:
                test_nums = 0
                for dic in res_lst[csv_lst[0]]:
                    if dic['bench'] == bench:
                        test_nums = dic['data'][6]
                print(f'{conf[0]} ({test_nums})\t', end = '&')
            elif row == 1: print(f'loc: {conf[1]}\t', end = '&')
            else: print("\t\t", end = '&')
            print(" "+unit_lst[row], end = '\t')
            for csv in csv_lst:
                for dic in res_lst[csv]:
                    if dic['bench'] == bench:
                        d = dic["data"][row]
                        if row == 0:
                            dp = dic["data"][5]
                            if dp == 0: print(f'& {d}', end = '\t')
                            else: print(f'& {d}({dp})', end = '\t')
                        elif row == 1:
                            dp = dic["data"][7]
                            if dp == 0: print(f'& {d:.2f}', end = '\t')
                            else: print(f'& {d:.2f}({dp})', end = '\t')
                        elif isinstance(d, float):
                            print(f'& {d:.2f}', end = '\t')
                        else: print(f'& {d}', end = '\t')
            print("\\\\")
        print("\hline")

def cal_solve_by_domain():
    """
        For Experiment 1 is how many benchmarks can be
        verified only with a specific abstract domain (regardless of which
        widening for that domain is being used).
    """
    truth_list = {"oct":[], "polka_st":[], "polka_ls":[]}
    # 1. Get all common res from each domain
    for domain, _ in can_solve_domain_dic.items():
        for x in range(0,2):
            domain_obj = copy.deepcopy(data_lst[csv_lst[0]])
            for dic in domain_obj:
                for i in range(0, len(dic["data"])):
                    dic["data"][i][2] = 'F'
            truth_list[domain].append(domain_obj)
    for domain, _ in can_solve_domain_dic.items():
        common = truth_list[domain]
        for file_name in csv_lst:
            domain_name = file_name.split('-')[1] # get comp domain
            tool_ver = 0 if file_name.split('-')[0] == "res" else 1
            if domain_name == domain:
                for dic1 in data_lst[file_name]:
                    for dic2 in common[tool_ver]:
                        if dic1["bench"] == dic2["bench"]:
                            for i in range(0, len(dic1["data"])):
                                if dic1["data"][i][2] == 'T':
                                    dic2["data"][i][2] = 'T'
    # 2. Compare the common results, generate the number
    for domain, res in can_solve_domain_dic.items():
        current_lst = truth_list[domain]
        for i in range(0, 2): # Get tool ver
                comp_domains = []
                for comp_domain, res_lst in truth_list.items():
                    if domain != comp_domain:
                        comp_domains.append(comp_domain)
                for dic1 in current_lst[i]:
                    for dic2 in truth_list[comp_domains[0]][i]:
                        if dic1["bench"] == dic2["bench"]:
                            for dic3 in truth_list[comp_domains[1]][i]:
                                if dic1["bench"] == dic3["bench"]:
                                    for j in range(0, len(dic1["data"])):
                                        if dic1["data"][j][2] == 'T' and dic2["data"][j][2] == 'F' and dic3["data"][j][2] == 'F':
                                            res[i][dic1["bench"]] += 1
    # print(can_solve_domain_dic)
    
    print("\t\tdomain | solvethis")
    benchlst = ["high", "first", "array", "list", "negative"]
    for i in range(0,2):
        if i == 0: print("non-sensitive")
        else: print("1-sensitive")
        for domain, res in can_solve_domain_dic.items():
            for bench in benchlst:
                print(f'{bench}: {domain} | {res[i][bench]}')


def print_unsolved_tests():
    for i in range(len(cant_solve_lst)):
        tool_ver = "nonsensitive" if i == 0 else "1-sensitive"
        print(f'-----{tool_ver}-----')
        for domain, vals in cant_solve_lst[i].items():
            print(f'{domain} ({len(vals)})', end = ":\t ")
            print(vals)
        print(f'-----end-----')


def main():
    read_data()
    cal_res()
    if exp2_regexp.search(args.folder_name):
        print_for_table2()
    else:
        cal_solve_by_domain()
        if args.show_unsolved:
            print_unsolved_tests()
        print_for_table1()

if __name__ == '__main__':
    main()