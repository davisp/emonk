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

SUBDIRS = priv

emonklibdir = $(localerlanglibdir)/emonk-$(version)
emonkebindir = $(emonklibdir)/ebin
emonkebin_DATA = $(compiled_files)

source_files = \
	src/emonk.app.in \
	src/emonk.erl \
	src/emonk_cache.erl \
	src/emonk_sup.erl

compiled_files = \
	ebin/emonk.app \
	ebin/emonk.beam \
	ebin/emonk_cache.beam \
	ebin/emonk_sup.beam
	
EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

ebin/emonk.app: src/emonk.app.tpl
	@mkdir -p ebin
	modules=`find ./src -name "*.erl" -exec basename {} .erl \; | tr '\n' ',' | sed "s/,$$//"`; \
	sed -e "s|%modules%|$$modules|g" > $@ < $<

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	$(ERLC) $(ERLC_FLAGS) -o ./ebin/ $<;

