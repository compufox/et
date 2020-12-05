QT          += widgets printsupport uitools
TEMPLATE    = app
CONFIG      += no_keywords release
INCLUDEPATH += /usr/local/include
EXTRA_LIBS  += -lasdf
#EXTRA_LIBS  += -lql-minitar
#EXTRA_LIBS  += -lecl-curl
EXTRA_LIBS  += -lsb-bsd-sockets
EXTRA_LIBS  += -lsockets
#EXTRA_LIBS  += -lserve-event
#EXTRA_LIBS  += -lecl-cdb
##EXTRA_LIBS  += -ldeflate
##EXTRA_LIBS  += -lecl-curl
LIBS        += -lecl -L. -let_lib -L/usr/local/lib -leql5 -L/usr/lib/ecl-20.4.24 $$EXTRA_LIBS
TARGET      = et
DESTDIR     = ./
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

win32 {
    include(../src/windows.pri)
}

SOURCES += main.cpp
