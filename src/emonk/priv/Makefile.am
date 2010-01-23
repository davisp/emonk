
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
emonkprivdir = $(emonk)/priv
emonkprivlibdir = $(emonk)/priv/lib

EMONK_SRCS = \
    emonk_drv.c \
	driver_comm.c \
	spidermonkey.c

emonkprivlib_LTLIBRARIES = emonk_drv.la
emonk_drv_la_SOURCES = $(EMONK_SRCS)
emonk_drv_la_LDFLAGS = -module -avoid-version
emonk_drv_la_LIBADD = @JSLIB@ -lnspr4

check: emonk_drv.so
emonk_drv.so: emonk_drv.la
	@rm -f $@
	$(LN_S) .libs/emonk_drv.so $@

install-data-hook:
	if test -f "$(DESTDIR)$(emonkprivlibdir)/emonk_drv"; then \
	    rm -f "$(DESTDIR)$(emonkprivlibdir)/emonk_drv.so"; \
	    cd "$(DESTDIR)$(emonkprivlibdir)" && $(LN_S) emonk_drv emonk_drv.so; \
	fi

uninstall-local:
	if test -f "$(DESTDIR)$(emonkprivlibdir)/emonk_drv"; then \
	    rm -f "$(DESTDIR)$(emonkprivlibdir)/emonk_drv.so"; \
	fi
