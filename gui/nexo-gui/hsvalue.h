#ifndef HSVALUE_H
#define HSVALUE_H

#include <HsFFI.h>
#include <QString>

class HsValue
{
public:
    enum ValueType
    {
        Num = 1,
        Bool = 2,
        Text = 3,
        Var = 4,
        List = 5,
        Record = 6,
        Fun = 7,
        Table = 8,
        Unknown = 0
    };

    HsValue(HsStablePtr value, ValueType type);
    HsValue(HsValue&& hsValue);
    ~HsValue();

    HsValue(const HsValue&) = delete;
    HsValue operator=(const HsValue&) = delete;

    ValueType getType();
    QString render() const;

    // warning: only use on list!
    QVector<HsValue *> toList() const;

    // warning: only use on table!
    QHash<QString, QVector<HsValue *>> toTable() const;

private:
    HsStablePtr value;
    ValueType type;
};

#endif // HSVALUE_H
