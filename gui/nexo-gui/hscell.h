#ifndef HSCELL_H
#define HSCELL_H

#include <HsFFI.h>

class QString;

class HsCell
{
public:
    enum WidgetType
    {
        ValueCell = 0,
        InputList = 1,
        Table = 2
    };

    explicit HsCell(HsStablePtr cell);
    ~HsCell();

    HsCell(const HsCell&) = delete;
    HsCell operator=(const HsCell&) = delete;

    HsCell(HsCell&& hsCell);
    HsCell& operator=(HsCell&& hsCell);

    QString name() const;
    QString type() const;
    WidgetType widgetType() const;
    QString exprAt(int row, int col) const;

    int cols() const;
    int rows() const;

private:
    HsStablePtr cell;
};

#endif // HSCELL_H
