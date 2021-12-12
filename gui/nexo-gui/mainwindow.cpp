#include "hssheet.h"
#include "inputlist.h"
#include "mainwindow.h"
#include "table.h"
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
    newTableAct = new QAction(tr("&Table"));

    connect(newValueAct, &QAction::triggered, this, &MainWindow::newValue);
    connect(newInputListAct, &QAction::triggered, this, &MainWindow::newInputList);
    connect(newTableAct, &QAction::triggered, this, &MainWindow::newTable);
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
    newMenu->addAction(newTableAct);

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

void MainWindow::newTable()
{
   Table *t = new Table(latestKey++, sheet);
   mdiArea->addSubWindow(t);
   t->show();
}

