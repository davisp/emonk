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

emonklibdir = $(localerlanglibdir)/emonk-$(version)
#emonkincludedir = $(emonklibdir)/include
emonkebindir = $(emonklibdir)/ebin

#emonkinclude_DATA = emonk.hrl
emonkebin_DATA = $(compiled_files)

CLEANFILES = $(compiled_files)

source_files = \
	emonk.app.in \
	emonk.erl \
	emonk_sup.erl \
	emonk_cache.erl \
	emonk_driver.erl

EXTRA_DIST = $(source_files)

compiled_files = \
	emonk.app \
	emonk.beam \
	emonk_sup.beam \
	emonk_cache.beam \
	emonk_driver.beam

emonk.app: emonk.app.tpl
	modules=`find . -name "*.erl" -exec basename {} .erl \; | tr '\n' ',' | sed "s/,$$//"`; \
	sed -e "s|%modules%|$$modules|g" $@ < $<
	chmod +x $@

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<;

