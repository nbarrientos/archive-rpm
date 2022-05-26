.PHONY: rpms rpm-source rpm-clean rpm-zstd rpm-gz rpm-xz \
	test test-interactive byte-compile

rpms: rpm-clean rpm-source rpm-zstd rpm-gz rpm-xz

rpm-clean:
	rm -rf ~/rpmbuild/SOURCES/* test/RPMS

rpm-source:
	mkdir -p ~/rpmbuild/SOURCES test/RPMS
	tar -C test -cvzf ~/rpmbuild/SOURCES/package-1.tgz package-1

rpm-zstd:
	rpmbuild -ba test/package.spec --define "_binary_payload w19.zstdio"
	mv ~/rpmbuild/RPMS/noarch test/RPMS/zstd

rpm-gz:
	rpmbuild -ba test/package.spec --define "_binary_payload w9.gzdio"
	mv ~/rpmbuild/RPMS/noarch test/RPMS/gz

rpm-xz:
	rpmbuild -ba test/package.spec --define "_binary_payload w6.xzdio"
	mv ~/rpmbuild/RPMS/noarch test/RPMS/xz

test: rpms
	make -C test test

test-interactive: rpms
	make -C test test-interactive

byte-compile:
	make -C test byte-compile
