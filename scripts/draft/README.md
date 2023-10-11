# benchmarking

# Testing the Drift tool-info Python module 

```
PATH=$PATH:/tools/home/eric/drift/ python3 -m benchexec.test_tool_info tools.drift --read-only-dir /
```

# Running benchmarking

```
cd /tools/home/eric/drift/scripts/draft
sudo benchexec benchmark-drift.xml --read-only-dir /
```

It will then tell you what `table-generator` command to run.

