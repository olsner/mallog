.PHONY: all clean

CXXFLAGS = -fpic -DPIC -fvisibility=hidden -O3 -fno-rtti -fno-exceptions -Wall
LDFLAGS = -lrt -ldl
LDSOFLAGS = $(LDFLAGS) -Wl,-no-undefined
GHCFLAGS = -O2 -fvia-c

all: mallog.so mallog2.so benchmark Convert

mallog.so: mallog.cpp
	$(CXX) $(CXXFLAGS) -shared -o $@ $< $(LDSOFLAGS)

mallog2.so: mallog2.cpp
	$(CXX) $(CXXFLAGS) -shared -o $@ $< $(LDSOFLAGS) #./mallog.so

%: %.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $<

Convert: Convert.hs
	ghc $(GHCFLAGS) --make Convert

clean:
	rm -f mallog.so
	rm -f mallog2.so
	rm -f benchmark

# Deps...
mallog.so: mallog2.h
mallog2.so: mallog2.h
