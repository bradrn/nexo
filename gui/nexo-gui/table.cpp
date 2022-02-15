#include "hscell.h"
#include "table.h"
#include "tabledelegate.h"
#include "tablemodel.h"

#include <QAction>
#include <QContextMenuEvent>
#include <QHeaderView>
#include <QLineEdit>
#include <QMenu>
#include <QTableView>
#include <QVBoxLayout>

Table::Table(int key, HsSheet *sheet, QWidget *parent)
    : QWidget(parent)
{
    model = new TableModel(key, sheet, parent);
    delegate = new TableDelegate();

    QVBoxLayout *l = new QVBoxLayout(this);

    nameEdit = new QLineEdit();
    l->addWidget(nameEdit);

    valueDisplay = new QTableView(this);
    valueDisplay->setModel(model);
    valueDisplay->setItemDelegate(delegate);
    valueDisplay->horizontalHeader()->hide();
    l->addWidget(valueDisplay);

    addRowAct = new QAction(tr("Add &row"));
    addColAct = new QAction(tr("Add &column"));

    connect(nameEdit, &QLineEdit::editingFinished, this, [=]() { model->setName(nameEdit->text()); });
    connect(addRowAct, &QAction::triggered, this, &Table::addRow);
    connect(addColAct, &QAction::triggered, this, &Table::addCol);
}

void Table::loadValueFrom(const HsCell &cell)
{
    nameEdit->setText(cell.name());
    model->loadValueFrom(cell);
}

void Table::contextMenuEvent(QContextMenuEvent *event)
{
    QMenu menu(this);
    menu.addAction(addRowAct);
    menu.addAction(addColAct);

    menu.exec(event->globalPos());
}

void Table::addRow()
{
    QModelIndex selected = valueDisplay->selectionModel()->currentIndex();
    if (!selected.isValid()) return;
    model->insertRow(selected.row()+1);
}

void Table::addCol()
{
    QModelIndex selected = valueDisplay->selectionModel()->currentIndex();
    if (!selected.isValid()) return;
    model->insertColumn(selected.column()+1);
}
