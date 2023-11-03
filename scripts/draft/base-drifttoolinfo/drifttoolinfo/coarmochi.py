# This file is part of BenchExec, a framework for reliable benchmarking:
# https://github.com/sosy-lab/benchexec
#
# SPDX-FileCopyrightText: 2007-2020 Dirk Beyer <https://www.sosy-lab.org>
# SPDX-FileCopyrightText: 2015 Carnegie Mellon University
#
# SPDX-License-Identifier: LicenseRef-BSD-3-Clause-CMU


# cd ~pepper/projects/coar/
# dune exec main -- -c ./config/solver/dbg_rcaml_pcsat_tb_ar.json -p ml ./benchmarks/OCaml/safety/simple/

# https://github.com/sosy-lab/benchexec/blob/main/doc/tool-integration.md
# https://github.com/sosy-lab/benchexec/blob/main/benchexec/tools/template.py

import benchexec.util as util
import benchexec.tools.template
import benchexec.result as result

import os


class Tool(benchexec.tools.template.BaseTool2):
    REQUIRED_PATHS = ["bin", "include", "lib", "share"]

    def executable(self,tool_locator):
        return tool_locator.find_executable("main.exe", subdir="/tools/home/eric/coar-clone-mihai/_build/default")

    def program_files(self, executable):
        return self._program_files_from_executable(
            executable, self.REQUIRED_PATHS, parent_dir=True
        )

    def name(self):
        return "coarmochi"

#  ./drift.exe -file tests/effects/traffic_light_fo_simple.ml -eff-aut tests/effects/traffic_light_fo_simple.eff 

    def working_directory(self, executable):
        return "/tools/home/eric/coar-clone-mihai/_build/default"

    def cmdline(self, executable, options, task, rlimits):
        #assert len(list(task.input_files)) == 1
        cmd = [executable] + options + [task.single_input_file]
        if task.property_file:
            print ("coarmochi.py: todo: cmdline property\n")
            cmd += ["-eff-aut", task.property_file]
        return cmd

    def version(self, executable):
        #return "0.0.todo"
        return self._version_from_tool(executable)

    #returncode, returnsignal, output, isTimeout):
    # see: https://github.com/sosy-lab/benchexec/blob/fde8a997ea8b522a73fedd3c2256140e635243ef/benchexec/result.py#L58
    def determine_result(self, run): 
        if "The input program is safe" in run.output:
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