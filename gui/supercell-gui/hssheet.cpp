#include "Interop_stub.h"
#include "hssheet.h"

#include <QString>
#include <tuple>

HsSheet::HsSheet()
{
    hsSheet = hsNewSheet();
}

void HsSheet::insertCell(int key, QString name, QString type, QString expr)
{
    bool *parseSuccess = new bool();
    HsStablePtr pexpr = hsParseExpr(expr.toUtf8().data(), parseSuccess);
    if (!*parseSuccess) return;

    HsStablePtr ptype = hsMaybeParseType(type.toUtf8().data());
    HsStablePtr cell = hsMkCell(name.toUtf8().data(), ptype, pexpr);
    hsInsert(key, cell, hsSheet);

    hsEvalSheet(hsSheet);
    emit reevaluated();
}

std::optional<QString> HsSheet::queryCell(int key)
{
    bool *qSuccess = new bool();
    HsStablePtr value = hsQuery(key, hsSheet, qSuccess);
    if (!*qSuccess) return {};

    QByteArray out = QByteArray((char*) hsDisplay(value));
    return QString::fromUtf8(out);
}
