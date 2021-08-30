#include "Interop_stub.h"
#include "hsvalue.h"

#include <QString>
#include <QVector>

HsValue::HsValue(HsStablePtr value, ValueType type)
    : value(value)
    , type(type)
{
}

HsValue::ValueType HsValue::getType()
{
    return type;
}

QString HsValue::render() const
{
    return QString::fromUtf8((char *) hsRenderValue(value));
}

QVector<HsValue> HsValue::toList() const
{
    int *len = new int;
    HsStablePtr *list = static_cast<HsStablePtr *>(hsValueToList(value, len));

    QVector<HsValue> result;
    for (int i=0; i<*len; ++i)
    {
        result.append(HsValue(list[i], Unknown));
    }

    return result;
}

/*
int HsValue::getListLength()
{
    return hsValueListLength(value);
}

HsValue HsValue::getValueAtIndex(int i)
{
    return HsValue(hsGetValueAtIndex(i, value), Unknown);
}
*/
