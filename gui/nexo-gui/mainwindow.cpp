#include "hscell.h"
#include "hssheet.h"
#include "inputlist.h"
#include "mainwindow.h"
#include "table.h"
#include "value.h"

#include <optional>
#include <QAction>
#include <QContextMenuEvent>
#include <QFileDialog>
#include <QMdiArea>
#include <QMdiSubWindow>
#include <QMenu>
#include <QMenuBar>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , mdiArea(new QMdiArea)
    , latestKey(0)
    , sheet(new HsSheet)
{
    mdiArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    mdiArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setCentralWidget(mdiArea);

    QMenu *fileMenu = menuBar()->addMenu("&File");
    fileMenu->addAction("&Open", this, &MainWindow::openSheet, QKeySequence::Open);
    fileMenu->addAction("&Save", this, &MainWindow::saveSheet, QKeySequence::Save);

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

void MainWindow::openSheet()
{
    QString fileName = QFileDialog::getOpenFileName(
                this,
                "Open",
                QString(),
                "Nexo sheets (*.nexo);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;
    QString sheetContents = QString::fromUtf8(file.readAll());
    file.close();

    if (std::optional<HsSheet *> openedSheet = HsSheet::parse(sheetContents))
    {
        QList<QMdiSubWindow *> subwindows = mdiArea->subWindowList();
        for (auto subwindow = subwindows.begin(); subwindow != subwindows.end(); ++subwindow)
        {
            mdiArea->removeSubWindow(*subwindow);
            delete *subwindow;
        }

        delete sheet;
        sheet = *openedSheet;

        sheet->reevaluate();
        sheet->disableInserts();

        QHash<int, HsCell *> cells = sheet->cells();
        for (QHash<int, HsCell *>::const_iterator cell = cells.constBegin(); cell != cells.constEnd(); ++cell)
        {
            const int &key = cell.key();
            HsCell &value = *cell.value();

            switch (value.widgetType())
            {
            case HsCell::WidgetType::ValueCell:
            {
                Value *v = new Value(cell.key(), sheet);
                v->loadValueFrom(value);
                mdiArea->addSubWindow(v);
                v->show();
                break;
            }
            case HsCell::WidgetType::InputList:
            {
                InputList *il = new InputList(cell.key(), sheet);
                il->loadValueFrom(value);
                mdiArea->addSubWindow(il);
                il->show();
                break;
            }
            case HsCell::WidgetType::Table:
            {
                Table *t = new Table(cell.key(), sheet);
                t->loadValueFrom(value);
                mdiArea->addSubWindow(t);
                t->show();
                break;
            }
            }
        }

        sheet->enableInserts();
    }
}

void MainWindow::saveSheet()
{
    QString fileName = QFileDialog::getSaveFileName(
                this,
                "Save",
                QString(),
                "Nexo sheets (*.nexo);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;
    file.write(sheet->render().toUtf8());
    file.close();
}

