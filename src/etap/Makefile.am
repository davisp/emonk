## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License.

noinst_DATA = $(compiled_files)

source_files = \
	src/etap.erl \
	src/etap_application.erl \
	src/etap_can.erl \
	src/etap_exception.erl \
	src/etap_process.erl \
	src/etap_report.erl \
	src/etap_request.erl \
	src/etap_string.erl \
	src/etap_web.erl

compiled_files = \
	ebin/etap.beam \
	ebin/etap_application.beam \
	ebin/etap_can.beam \
	ebin/etap_exception.beam \
	ebin/etap_process.beam \
	ebin/etap_report.beam \
	ebin/etap_request.beam \
	ebin/etap_string.beam \
	ebin/etap_web.beam

EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	$(ERLC) $(ERLC_FLAGS) -o ./ebin/ $<;

