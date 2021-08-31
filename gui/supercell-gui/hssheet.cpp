#include "Interop_stub.h"
#include "hssheet.h"
#include "hsvalue.h"

#include <QString>
#include <tuple>

HsSheet::HsSheet()
{
    hsSheet = hsNewSheet();
}

HsSheet::~HsSheet()
{
    hs_free_stable_ptr(hsSheet);
}

void HsSheet::insertCell(int key, QString name, QString type, QString expr)
{
    bool *parseSuccess = new bool();
    HsStablePtr pexpr = hsParseExpr(expr.toUtf8().data(), parseSuccess);
    if (!*parseSuccess) return;

    HsStablePtr ptype = hsMaybeParseType(type.toUtf8().data());
    HsStablePtr cell = hsMkCell(name.toUtf8().data(), ptype, pexpr);
    hsInsert(key, cell, hsSheet);

    hs_free_stable_ptr(cell);
    hs_free_stable_ptr(ptype);
    hs_free_stable_ptr(pexpr);
    delete parseSuccess;

    hsEvalSheet(hsSheet);
    emit reevaluated();
}

std::variant<std::monostate, QString, HsValue> HsSheet::queryCell(int key)
{
    bool *qSuccess = new bool();
    HsStablePtr value = hsQuery(key, hsSheet, qSuccess);
    if (!*qSuccess) return {};

    if (auto error = (char *) hsDisplayError(value); *error)
        return QString::fromUtf8(error);
    else
        return HsValue(hsExtractValue(value), static_cast<HsValue::ValueType>(hsExtractTopLevelType(value)));

    hs_free_stable_ptr(value);
    delete qSuccess;
}
