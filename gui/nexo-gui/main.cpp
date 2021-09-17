#include "mainwindow.h"

#include <HsFFI.h>
#include <QApplication>

int main(int argc, char *argv[])
{
    hs_init(&argc, &argv);

    QApplication a(argc, argv);
    MainWindow w;
    w.show();
    int retval = a.exec();

    hs_exit();
    return retval;
}
