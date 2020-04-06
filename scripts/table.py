# -*- coding: utf-8 -*-
######################################################################
# Copyright 2019 Yusen Su. All Rights Reserved.
# Net id: ys3547
######################################################################
import sys, os
import re
import csv

testdir = '../outputs/DART_IT'
table_file = '../res.csv'
pattern = re.compile(".*ml$")
attrs = ['subdir', 'file name', 'test class', 'timing (seconds)', 'res (true/false)', 'domain']

def manipulate_input_data(data):
    res_data = []
    data = data[2:]
    temp_data = ""
    flag = False
    for x in data:
        if x == '\n' and flag:
            flag = False
            res_data.append(temp_data)
            temp_data = ''
        elif x == '\n' and not flag:
            flag = True
            continue
        else:
            temp_data += x
    res_data.append(temp_data)
    return res_data


def read_info_from_file(file_name):
    try:
        file = open(file_name, 'r')
    except:
        sys.exit("index file does not exits. Exit!")
    data = file.readlines()
    res_data = []
    plain_data = manipulate_input_data(data)
    plain_data = [d.replace(" ", "").replace("\t", "").replace("\n", "") for d in plain_data]
    _, domain_name = plain_data[0].split(":")
    res_data.append(domain_name)
    _, timing = plain_data[-1].split(":")
    if len(plain_data) < 3:
        res_data.append('ER')
        res_data.append('error')
    else:
        plain_d = plain_data[1]
        unit_regexp = re.compile(r'Unit')
        int_regexp = re.compile(r'Int')
        bool_regexp = re.compile(r'Bool')
        bot_regexp = re.compile(r'_|_')
        pref_regexp = re.compile(r'PreDef')
        if pref_regexp.search(plain_d):
            res_data.append('ER')
        elif unit_regexp.search(plain_d):
            res_data.append('T')
        elif int_regexp.search(plain_d):
            value = plain_d.split('|')[2]
            if re.match("^bottom", value, re.IGNORECASE):
                res_data.append('N')
            else:
                res_data.append('INT')
        elif bool_regexp.search(plain_d):
            true_part, false_part = plain_d.split(',')
            _, bot_test_f = false_part.split(':')
            bot_test_t = true_part.split(':')[2]
            if re.match("^bottom", bot_test_f, re.IGNORECASE):
                res_data.append('T')
            elif re.match("^bottom", bot_test_t, re.IGNORECASE):
                res_data.append('F')
            else:
                res_data.append('F')
        elif bot_regexp.search(plain_d):
            res_data.append('T') # Unreachable
        else:
            res_data.append('UN')
        time = float(timing) / 1000
        res_data.append(round(time,2))
    return res_data

def read_false_info_from_file(file_name):
    try:
        file = open(file_name, 'r')
    except:
        sys.exit("index file does not exits. Exit!")
    data = file.readlines()
    res_data = []
    plain_data = manipulate_input_data(data)
    plain_data = [d.replace(" ", "").replace("\t", "").replace("\n", "") for d in plain_data]
    _, domain_name = plain_data[0].split(":")
    res_data.append(domain_name)
    _, timing = plain_data[-1].split(":")
    if len(plain_data) < 3:
        res_data.append('ER')
        res_data.append('error')
    else:
        plain_d = plain_data[1]
        unit_regexp = re.compile(r'Unit')
        int_regexp = re.compile(r'Int')
        bool_regexp = re.compile(r'Bool')
        bot_regexp = re.compile(r'_|_')
        if unit_regexp.search(plain_d):
            res_data.append('T') # True Negative
        elif int_regexp.search(plain_d):
            value = plain_d.split('|')[2]
            if re.match("^bottom", value, re.IGNORECASE):
                res_data.append('F')
            else:
                res_data.append('T')
        elif bool_regexp.search(plain_d):
            true_part, false_part = plain_d.split(',')
            _, bot_test_f = false_part.split(':')
            bot_test_t = true_part.split(':')[2]
            if re.match("^bottom", bot_test_f, re.IGNORECASE) and re.match("^bottom", bot_test_t, re.IGNORECASE):
                res_data.append('T')
            elif re.match("^bottom", bot_test_t, re.IGNORECASE):
                res_data.append('T')
            else:
                res_data.append('F')
        elif bot_regexp.search(plain_d):
            res_data.append('F') # Unreachable
        else:
            res_data.append('UN')
        time = float(timing) / 1000
        res_data.append(round(time,2))
    return res_data

def dispatch(subdir, dict):
    for root, dirs, files in os.walk(testdir+'/'+subdir):
        dict[subdir] = [(os.path.join(root, file), file, root) for file in files if pattern.match(file)]
    for file_path in dict[subdir]:
        _, dir = file_path[2].split('../outputs/DART_IT/')
        if dir == "negative":
            res_data = read_false_info_from_file(file_path[0])
            res_data.append("Neg")
        else:
            res_data = read_info_from_file(file_path[0])
            res_data.append("Pos")
        test_case = file_path[1][4:-3]
        res_data.append(test_case)
        res_data.append(subdir)
        res_data.reverse()
        dict[subdir][dict[subdir].index(file_path)] = res_data

def search_all_outputs_file(table_dict):
    subdirs = next(os.walk(testdir))[1]
    for subdir in subdirs:
        dispatch(subdir, table_dict)

def write_info_into_csv(table_dict):
    try:
        file = open(table_file, 'w')
    except:
        sys.exit("output table file does not exits. Exit!")
    strs = ','.join(attrs)
    file.write(strs+'\n')
    file.close()
    with open(table_file, 'a', newline='') as csvfile:
        writer = csv.writer(csvfile, delimiter=',',
                        quotechar='|', quoting=csv.QUOTE_MINIMAL)
        for k in table_dict.keys():
            for d in table_dict[k]:
                writer.writerow(d)

def main():
    table_dict = {}
    search_all_outputs_file(table_dict)
    write_info_into_csv(table_dict)

if __name__ == '__main__':
	main()
