# benchmarking

# 1. Building the python package

```
# AS ROOT!
cd /tools/home/eric/drift/scripts/draft/base-drifttoolinfo
sudo pip uninstall drifttoolinfo
sudo python3 setup.py sdist bdist_wheel
sudo pip install .
```

# 2. Testing the Drift tool-info Python module 

```
# As ROOT!
PATH=$PATH:/tools/home/eric/drift/ python3 -m benchexec.test_tool_info drifttoolinfo.drift --read-only-dir /
```

# 3. Running benchmarking

```
cd /tools/home/eric/drift/scripts/draft
sudo benchexec benchmark-drift.xml --read-only-dir / --limitCores 2 --timelimit 15s --numOfThreads 8
```

It will then tell you what `table-generator` command to run.

