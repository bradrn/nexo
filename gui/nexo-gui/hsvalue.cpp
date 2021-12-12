#include "Interop_stub.h"
#include "hsvalue.h"

#include <QHash>
#include <QString>
#include <QVector>

HsValue::HsValue(HsStablePtr value, ValueType type)
    : value(value)
    , type(type)
{
}

HsValue::HsValue(HsValue &&hsValue)
    : value(hsValue.value)
    , type(hsValue.type)
{
    hsValue.value = hsNullStablePtr();
}

HsValue::~HsValue()
{
    hs_free_stable_ptr(value);
}

HsValue::ValueType HsValue::getType()
{
    return type;
}

QString HsValue::render() const
{
    return QString::fromUtf8((char *) hsRenderValue(value));
}

QVector<HsValue *> HsValue::toList() const
{
    int *len = new int;
    HsStablePtr *list = static_cast<HsStablePtr *>(hsValueToList(value, len));

    QVector<HsValue *> result;
    for (int i=0; i<*len; ++i)
    {
        result.append(new HsValue(list[i], Unknown));
    }

    delete len;

    return result;
}

QHash<QString, QVector<HsValue *> > HsValue::toTable() const
{
    int *len = new int;
    char ***cheaders = new char**;
    int **collens = new int*;
    HsStablePtr **table = static_cast<HsStablePtr **>(
                hsValueToTable(value, len, cheaders, collens));

    QHash<QString, QVector<HsValue *> > result;
    for (int i=0; i<*len; ++i)
    {
        QString header = QString::fromUtf8((*cheaders)[i]);
        QVector<HsValue *> col;
        for (int j=0; j<(*collens)[i]; ++j)
        {
            col.append(new HsValue(table[i][j], Unknown));
        }
        result.insert(header, col);
    }

    for (int i=0; i<*len; ++i)
    {
        free((*cheaders)[i]);
    }

    free(*collens);

    delete cheaders;
    delete collens;
    delete len;

    return result;
}
