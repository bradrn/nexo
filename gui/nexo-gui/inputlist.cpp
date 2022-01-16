#include "hscell.h"
#include "hssheet.h"
#include "inputlist.h"

#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QListView>
#include <QPushButton>
#include <QStringListModel>

InputList::InputList(int key, HsSheet *sheet, QWidget *parent)
    : QWidget(parent)
    , key(key)
    , sheet(sheet)
{
    QGridLayout *l = new QGridLayout(this);

    l->addWidget(new QLabel("Name"), 0, 0);
    l->addWidget(new QLabel("Type"), 1, 0);
    l->addWidget(new QLabel("Input:"), 2, 0, 1, 2);

    nameEdit = new QLineEdit();
    l->addWidget(nameEdit, 0, 1);

    typeEdit = new QLineEdit();
    l->addWidget(typeEdit, 1, 1);

    model = new QStringListModel();

    valueDisplay = new QListView();
    valueDisplay->setModel(model);
    l->addWidget(valueDisplay, 3, 0, 1, 2);

    QPushButton *add = new QPushButton("+");
    l->addWidget(add, 4, 0);

    connect(nameEdit, &QLineEdit::editingFinished, this, &InputList::invalidate);
    connect(typeEdit, &QLineEdit::editingFinished, this, &InputList::invalidate);
    connect(model, &QStringListModel::dataChanged, this, &InputList::invalidate);

    connect(add, &QPushButton::clicked, this, &InputList::addItem);
}

void InputList::loadValueFrom(const HsCell &cell)
{
    nameEdit->setText(cell.name());
    typeEdit->setText(cell.type());

    int rows = cell.rows();
    QStringList exprs;
    for (int row=0; row<rows; ++row)
        exprs << cell.exprAt(row, 0);
    model->setStringList(exprs);
}

void InputList::invalidate()
{
    sheet->insertLiteralList(key, nameEdit->text(), typeEdit->text(), model->stringList());
}

void InputList::addItem()
{
    model->insertRow(model->rowCount());
    QModelIndex i = model->index(model->rowCount()-1);
    model->setData(i, "0");
}
