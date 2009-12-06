.PHONY: all clean

CXXFLAGS = -fpic -DPIC -fvisibility=hidden -O3 -fno-rtti -fno-exceptions -Wall
LDFLAGS = -lrt -ldl
LDSOFLAGS = $(LDFLAGS) -Wl,-no-undefined
GHCFLAGS = -O2 -fvia-c

all: mallog.so benchmark Convert

mallog.so: mallog.cpp
	$(CXX) $(CXXFLAGS) -shared -o $@ $< $(LDSOFLAGS)

%: %.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $<

Convert: Convert.hs
	ghc $(GHCFLAGS) --make Convert

clean:
	rm -f mallog.so
	rm -f benchmark
