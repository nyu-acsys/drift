# Artifact for "Abstract Interpretation of Temporal Safety Effects of Higher Order Programs"

This artifact includes the complete source code and all necessary materials to run the experiments and reproduce the results presented in Table 1 (Section 8) and Table 2 (Appendix).

We claim the available and reusable badges for `evDrift`.


## Getting started

First, you need to make sure `Docker` is installed on your machine. Documentation can be found here:  https://docs.docker.com/engine/install/

### Mac users
Use Docker Desktop version v4.34.3 or later, and enable "Use Rosetta for x86_64/amd64 emulation on Apple Silicon" in Settings->General.

The artifact uses Docker to run all experiments. Ensure you have at least 20GB of available disk space. The uncompressed Docker image is approximately 8GB and includes the Git repositories used to build all tools included in the comparison. 

A compressed archive containing the pre-built Docker image is available at zenodo as oopsla25.tar.xz. Use the following command to extract it:
```bash
tar -xJvf oopsla25.tar.xz
```

Load the docker image `oopsla25-evdrift` from the tar archive
```bash
docker load -i oopsla25-evdrift.tar
```

Run the container with
```bash
docker run --init -d oopsla25-evdrift tail -f /dev/null
```
This command starts and keep the container running indefinitely in the background. You can see its name by executing `docker container ls` and attach to it using:
```bash
docker exec -it <container_id_or_name> /bin/bash
```
Once you are inside the container--the above command places you in a bash environment--you can inspect the source code and run the experiments. 

When finished, you can stop and remove the container by passing the command `docker rm -f <container_id_or_name>`.

## Smoke test

To run a subset of the experiments, use 
```bash
python3 run_expeeriments.py smoke
```
This will detect any technical issues during the smoke test phase. 
If finished successfully, the evaluation script should print
```
name        drift   rcaml   mochi   evDrift
-------------------------------------------
all-ev-pos  verif.  verif.  verif.  verif. 
depend      verif.  verif.  verif.  verif. 
overview1   verif.  verif.  verif.  verif. 
```

## Running full experiments 

The sources can be found in `/oopsla25/evDrift/tests/effects/*.ml`, along with properties
in `/oopsla25/evDrift/tests/effects/*.yml.prp`. Unsafe versions of the benchmarks are 
found in `/oopsla25/evDrift/tests/effects/unsafe/*.ml`.

The scripts to re-produce the experiments that are reported in Table 1
and Table 2 are described below. Note that we have used BenchExec
(https://github.com/sosy-lab/benchexec/), which is a nearly industral-level
benchmarking tool, that uses Linux CGroups to measure CPU and memory
usage with high precision. We use BenchExec to run Drift and evDrift across
all possible configurations. If interested, these full experiments
can be reproduced according to the instructions found in
`/oopsla25/evDrift/scripts/effects/README.md`.

To avoid the need to install and configure BenchExec, we have created
a script that simply run the best-performing configurations, which are
those reported in Table 1.
The results of the each run can be found at `/oopsla25/evDrift/result/<benchmark_name>_<tool_name>.txt`.
To reproduce Table 1 results, run:

```
python3 run_expeeriments.py all
```

## Reusability
The source code for `evDrift` is located in `/oopsla25/evDrift` directory of the container and on Github at https://github.com/nyu-acsys/drift

See the `Overview.md` file for complete installation instructions across platforms and guidance on running the tool with different configuration options.


