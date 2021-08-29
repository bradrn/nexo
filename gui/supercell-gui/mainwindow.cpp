#include "hssheet.h"
#include "mainwindow.h"
#include "value.h"

#include <QAction>
#include <QContextMenuEvent>
#include <QMdiArea>
#include <QMenu>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , mdiArea(new QMdiArea)
    , values()
    , latestKey(0)
    , sheet(new HsSheet)
{
    mdiArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    mdiArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setCentralWidget(mdiArea);

    newValueAct = new QAction(tr("&Value"));
    connect(newValueAct, &QAction::triggered, this, &MainWindow::newValue);
}

MainWindow::~MainWindow()
{
}

void MainWindow::contextMenuEvent(QContextMenuEvent *event)
{
    QMenu menu(this);

    QMenu *newMenu = menu.addMenu("New");
    newMenu->addAction(newValueAct);

    menu.exec(event->globalPos());
}

void MainWindow::newValue()
{
    Value *v = new Value(latestKey++, sheet);
    mdiArea->addSubWindow(v);
    values.append(v);
    v->show();
}

