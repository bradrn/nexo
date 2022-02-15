#include "Interop_stub.h"
#include "hscell.h"

#include <QString>

HsCell::HsCell(HsStablePtr cell)
    : cell(cell)
{
}

HsCell::~HsCell()
{
    hs_free_stable_ptr(cell);
}

HsCell::HsCell(HsCell&& hsCell)
    : cell(hsCell.cell)
{
    hsCell.cell = hsNullStablePtr();
}

HsCell &HsCell::operator=(HsCell &&hsCell)
{
    if (&hsCell == this) return *this;

    hs_free_stable_ptr(cell);
    cell = hsCell.cell;
    hsCell.cell = hsNullStablePtr();

    return *this;
}

QString HsCell::name() const
{
    return QString::fromUtf8(static_cast<char *>(hsCellName(cell)));
}

QString HsCell::type() const
{
    return QString::fromUtf8(static_cast<char *>(hsCellType(cell)));
}

HsCell::WidgetType HsCell::widgetType() const
{
    return static_cast<WidgetType>(hsWidgetType(cell));
}

QString HsCell::exprAt(int row, int col) const
{
    return QString::fromUtf8(static_cast<char *>(hsExprAt(row, col, cell)));
}

QString HsCell::typeOf(QString col) const
{
    return QString::fromUtf8(static_cast<char *>(
        hsCellTypeOfColumn(col.toUtf8().data(), cell)));
}

int HsCell::cols() const
{
    return hsWidgetCols(cell);
}

int HsCell::rows() const
{
    return hsWidgetRows(cell);
}
