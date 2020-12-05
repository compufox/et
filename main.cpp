#undef SLOT

#include <ecl/ecl.h>
#include <eql5/eql.h>
#include <QApplication>
#include <QTextCodec>
#include <QSettings>
#include <QTranslator>

// adapt "ET" (here: from "et.asd")
extern "C" {
  void init_lib_ET__ALL_SYSTEMS(cl_object);
  void init_lib_ASDF(cl_object);
  //  void init_lib_QL_MINITAR(cl_object);
  void init_lib_SOCKETS(cl_object);
  void init_lib_SB_BSD_SOCKETS(cl_object);
  //  void init_lib_SERVE_EVENT(cl_object);
  //  void init_lib_ECL_CDB(cl_object);
  //  void init_lib_ECL_HELP(cl_object);
  //  void init_lib_DEFLATE(cl_object);
  //  void init_lib_ECL_CURL(cl_object);
}

int catch_all_qexec() {
    int ret = 0;
    CL_CATCH_ALL_BEGIN(ecl_process_env()) {
        ret = QApplication::exec(); }
    CL_CATCH_ALL_END;
    return ret; }

int main(int argc, char** argv) {

    EQL::ini(argv); // best initialized here

    QCoreApplication::setAttribute(Qt::AA_ShareOpenGLContexts); // for Qt WebEngine
    QApplication qapp(argc, argv);

    QTextCodec* utf8 = QTextCodec::codecForName("UTF-8");
    QTextCodec::setCodecForLocale(utf8);

    // Hint: use QSettings or similar to store your language settings.
    // It is put here because it must load _before_ the Lisp code is executed.
    // You'll probably find a more elegant solution by yourself.
    QString language("es"); // example: spanish
    QTranslator tr, trQt;
    if(tr.load("eql_" + language)) {
        qapp.installTranslator(&tr);
        if(trQt.load("qt_" + language)) {
            qapp.installTranslator(&trQt); }}

    EQL eql;

#ifdef Q_OS_WIN
    // print output would crash program
    eql.ignoreIOStreams();
#endif

    //    ecl_init_module(NULL, init_lib_ECL_CURL);
    //    ecl_init_module(NULL, init_lib_DEFLATE);
    //    ecl_init_module(NULL, init_lib_ECL_HELP);
    //    ecl_init_module(NULL, init_lib_ECL_CDB);
    //    ecl_init_module(NULL, init_lib_SERVE_EVENT);
    ecl_init_module(NULL, init_lib_SOCKETS);
    ecl_init_module(NULL, init_lib_SB_BSD_SOCKETS);
    //    ecl_init_module(NULL, init_lib_QL_MINITAR);
    ecl_init_module(NULL, init_lib_ASDF);

    // adapt "ET" (here: from "et.asd")
    eql.exec(init_lib_ET__ALL_SYSTEMS);

    return catch_all_qexec(); } // closing the main/last window will quit the program
