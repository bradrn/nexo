#include "hscell.h"
#include "hssheet.h"
#include "hsvalue.h"
#include "value.h"

#include <QAbstractItemView>
#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QListView>
#include <QStringListModel>
#include <QTextEdit>

Value::Value(int key, HsSheet *sheet, QWidget *parent)
    : QWidget(parent)
    , key(key)
    , sheet(sheet)
{
    QGridLayout *l = new QGridLayout(this);
    layout = l;

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

    QLineEdit *valueEdit = new QLineEdit();
    valueEdit->setReadOnly(true);
    valueEdit->setFocusPolicy(Qt::NoFocus);

    valueDisplay = valueEdit;
    l->addWidget(valueDisplay, 3, 1);

    connect(nameEdit, &QLineEdit::editingFinished, this, &Value::invalidate);
    connect(typeEdit, &QLineEdit::editingFinished, this, &Value::invalidate);
    connect(exprEdit, &QLineEdit::editingFinished, this, &Value::invalidate);

    connect(sheet, &HsSheet::reevaluated, this, &Value::requery);

    invalidate();
}

void Value::loadValueFrom(const HsCell &cell)
{
    nameEdit->setText(cell.name());
    typeEdit->setText(cell.type());
    exprEdit->setText(cell.exprAt(0,0));
    requery();
}

void Value::invalidate()
{
    sheet->insertCell(key, nameEdit->text(), typeEdit->text(), exprEdit->text());
}

void Value::requery()
{
    auto result = sheet->queryValue(key);

    if (QString *msg = std::get_if<QString>(&result))
    {
        setErrorText(*msg);
    }
    else if (HsValue *value = std::get_if<HsValue>(&result))
    {
        switch(value->getType())
        {
        case HsValue::ValueType::List:
        {
            QVector<HsValue *> values = value->toList();
            if (!values.empty()) {
                QStringList valuesStr;
                for (auto i = values.constBegin(); i != values.constEnd(); ++i)
                    valuesStr.append((*i)->render());

                QStringListModel *model = new QStringListModel(valuesStr);
                auto valueEdit = getEditWidget<QListView>();
                valueEdit->setModel(model);
                valueEdit->setEditTriggers(QAbstractItemView::NoEditTriggers);

                break;
            }
            // else fall through if list is null
        }
        default:
            setValueText(value->render());
            break;
        }
    }

    return;
}

void Value::setValueText(QString valueText)
{
    auto valueEdit = getEditWidget<QLineEdit>();
    valueEdit->setReadOnly(true);
    valueEdit->setText(valueText);
}

void Value::setErrorText(QString errorText)
{
    auto errorEdit = getEditWidget<QTextEdit>();
    errorEdit->setReadOnly(true);
    errorEdit->setText(errorText);
}

template<class W>
W* Value::getEditWidget()
{
    W *w;
    if (auto e = dynamic_cast<W *>(valueDisplay))
    {
        w = e;
    }
    else
    {
        w = new W();
        w->setFocusPolicy(Qt::NoFocus);
        delete layout->replaceWidget(valueDisplay, w);
        delete valueDisplay;
        valueDisplay = w;
    }

    return w;
}
