#include "hssheet.h"
#include "inputlist.h"
#include "mainwindow.h"
#include "value.h"

#include <QAction>
#include <QContextMenuEvent>
#include <QMdiArea>
#include <QMenu>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , mdiArea(new QMdiArea)
    , latestKey(0)
    , sheet(new HsSheet)
{
    mdiArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    mdiArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setCentralWidget(mdiArea);

    newValueAct = new QAction(tr("&Value"));
    newInputListAct = new QAction(tr("&Input List"));

    connect(newValueAct, &QAction::triggered, this, &MainWindow::newValue);
    connect(newInputListAct, &QAction::triggered, this, &MainWindow::newInputList);
}

MainWindow::~MainWindow()
{
}

void MainWindow::contextMenuEvent(QContextMenuEvent *event)
{
    QMenu menu(this);

    QMenu *newMenu = menu.addMenu("New");
    newMenu->addAction(newValueAct);
    newMenu->addAction(newInputListAct);

    menu.exec(event->globalPos());
}

void MainWindow::newValue()
{
    Value *v = new Value(latestKey++, sheet);
    mdiArea->addSubWindow(v);
    v->show();
}

void MainWindow::newInputList()
{
    InputList *il = new InputList(latestKey++, sheet);
    mdiArea->addSubWindow(il);
    il->show();
}

