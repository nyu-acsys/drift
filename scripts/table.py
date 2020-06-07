# -*- coding: utf-8 -*-
######################################################################
# Copyright 2019 Yusen Su. All Rights Reserved.
# Net id: ys3547
######################################################################
import sys, os
import re
import csv
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('-csv', type=str, help='give an output csv name', required=True)
args = parser.parse_args()

testdir = '../outputs/DRIFT2'
table_file = '../' + args.csv + '.csv'
pattern = re.compile(".*ml$")
attrs = ['subdir', 'file name', 'test class', 'timing (seconds)', 'res (true/false)', 'domain']

def manipulate_input_data(data):
    res_data = []
    for x in data:
        if x == '\n':
            continue
        else:
            res_data.append(x)
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
    else:
        plain_d = plain_data[1]
        safe_regexp = re.compile(r'safe')
        unsafe_regexp = re.compile(r'failed')
        timeout_regexp = re.compile(r'timeout')
        if unsafe_regexp.search(plain_d):
            res_data.append('F')
        elif safe_regexp.search(plain_d):
            res_data.append('T')
        elif timeout_regexp.search(plain_d):
            res_data.append('F')
        else:
            print(file_name+" File data error: \n")
            sys.exit(data)
    try:
        time = float(timing) / 1000
        res_data.append(round(time,2))
    except:
        res_data.append(timing)
    return res_data

"""
if an assertion is supposed to fail and the tool
detects this correctly (regardless of whether there is a precision
loss), then this should count as "success". 
"""
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
    else:
        plain_d = plain_data[1]
        safe_regexp = re.compile(r'safe')
        unsafe_regexp = re.compile(r'failed')
        timeout_regexp = re.compile(r'timeout')
        if unsafe_regexp.search(plain_d):
            res_data.append('T')
        elif safe_regexp.search(plain_d):
            res_data.append('F')
        elif timeout_regexp.search(plain_d):
            res_data.append('F')
        else:
            print("File data error: \n")
            sys.exit(data)
    try:
        time = float(timing) / 1000
        res_data.append(round(time,2))
    except:
        res_data.append(timing)
    return res_data

def dispatch(subdir, dict):
    for root, dirs, files in os.walk(testdir+'/'+subdir):
        dict[subdir] = [(os.path.join(root, file), file, root) for file in files if pattern.match(file)]
    for file_path in dict[subdir]:
        _, dir = file_path[2].split('../outputs/DRIFT2/')
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
