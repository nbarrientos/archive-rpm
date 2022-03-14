Name: package
Version: 1
Release: 1%{?dist}
Summary: Test RPM
License: BSD
BuildArch: noarch
Source: %{name}-%{version}.tgz
URL: https://example.org
Vendor: ACME Inc.

%description
RPM for testing

%prep
%setup -n %{name}-%{version}

%build
rm -rf %{buildroot}

%install

%files
%defattr(-,root,root,-)
%doc README.md
