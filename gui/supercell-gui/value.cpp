#include "hssheet.h"
#include "value.h"

#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>

Value::Value(int key, HsSheet *sheet, QWidget *parent)
    : QWidget(parent)
    , key(key)
    , sheet(sheet)
{
    QGridLayout *l = new QGridLayout(this);

    l->addWidget(new QLabel("Name"), 0, 0);
    l->addWidget(new QLabel("Type"), 1, 0);
    l->addWidget(new QLabel("Expr"), 2, 0);
    l->addWidget(new QLabel("Value"), 3, 0);

    nameEdit = new QLineEdit();
    l->addWidget(nameEdit, 0, 1);

    typeEdit = new QLineEdit();
    l->addWidget(typeEdit, 1, 1);

    exprEdit = new QLineEdit();
    l->addWidget(exprEdit, 2, 1);

    valueEdit = new QLineEdit();
    valueEdit->setReadOnly(true);
    l->addWidget(valueEdit, 3, 1);

    connect(nameEdit, &QLineEdit::editingFinished, this, &Value::invalidate);
    connect(typeEdit, &QLineEdit::editingFinished, this, &Value::invalidate);
    connect(exprEdit, &QLineEdit::editingFinished, this, &Value::invalidate);
    connect(valueEdit, &QLineEdit::editingFinished, this, &Value::invalidate);

    connect(sheet, &HsSheet::reevaluated, this, &Value::requery);

    invalidate();
}

void Value::invalidate()
{
    sheet->insertCell(key, nameEdit->text(), typeEdit->text(), exprEdit->text());
}

void Value::requery()
{
    if (auto value = sheet->queryCell(key))
        valueEdit->setText(*value);
}
