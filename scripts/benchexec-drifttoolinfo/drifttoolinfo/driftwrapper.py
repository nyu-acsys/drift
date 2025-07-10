# This file is part of BenchExec, a framework for reliable benchmarking:
# https://github.com/sosy-lab/benchexec
#
# SPDX-FileCopyrightText: 2007-2020 Dirk Beyer <https://www.sosy-lab.org>
# SPDX-FileCopyrightText: 2015 Carnegie Mellon University
#
# SPDX-License-Identifier: LicenseRef-BSD-3-Clause-CMU

# https://github.com/sosy-lab/benchexec/blob/main/doc/tool-integration.md
# https://github.com/sosy-lab/benchexec/blob/main/benchexec/tools/template.py

import benchexec.util as util
import benchexec.tools.template
import benchexec.result as result

import os
import re

# This invokes Drift with the perl wrapper that loads the best configuration
#.   for the benchmark:
# ./drift_benchmark.pl best_configs_drift.csv <benchmark>

class Tool(benchexec.tools.template.BaseTool2):
    REQUIRED_PATHS = ["bin", "include", "lib", "share"]

    def executable(self,tool_locator):
        return tool_locator.find_executable("drift_benchmark.pl", subdir="/usr/local/bin")

    def program_files(self, executable):
        return self._program_files_from_executable(
            executable, self.REQUIRED_PATHS, parent_dir=True
        )

    def name(self):
        return "drift_benchmark"

#  ./drift.exe -file tests/effects/traffic_light_fo_simple.ml -eff-aut tests/effects/traffic_light_fo_simple.eff 

    def cmdline(self, executable, options, task, rlimits):

        cmd = [executable] + options.get("-cfg")

        #if task.options is not None and "data_model" in task.options:
        #    options += ["--arch", task.options.get("data_model")]

        return cmd

    def version(self, executable):
        return "0.1"
        #return self._version_from_tool(executable)

    # Same as the normal drift drifttoolinfo
    def determine_result(self, run): 

        p_notsfe = re.compile('The program may not be safe')
        for line in reversed(run.output):
            if p_notsfe.match(line):
                return result.RESULT_UNKNOWN
            
        if "The input program is safe." in run.output:
            status = result.RESULT_TRUE_PROP
        elif "The program may not be safe" in run.output:
            status = result.RESULT_UNKNOWN
            # if "BRUNCH_STAT Termination" in output:
            #     status = result.RESULT_FALSE_TERMINATION
            # else:
            #     status = result.RESULT_FALSE_REACH
        elif run.was_terminated:
            status = result.RESULT_UNKNOWN
        elif run.exit_code.signal == 9 or run.exit_code.signal == (128 + 9):
            if run.was_timeout:
                status = result.RESULT_TIMEOUT
            else:
                status = "KILLED BY SIGNAL 9"
        elif run.exit_code.value != 0:
            status = f"ERROR ({run.exit_code.value})"
        else:
            status = "FAILURE"

        return status