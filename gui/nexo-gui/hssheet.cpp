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
    if (!*parseSuccess)
    {
        hs_free_stable_ptr(pexpr);
        delete parseSuccess;
        return;
    }

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

void HsSheet::insertLiteralList(int key, QString name, QString type, QStringList lits)
{
    int length = lits.length();
    char **clist = new char*[length];
    for (int i=0; i<length; i++)
    {
        QString lit = lits[i];
        clist[i] = new char[lit.length()+1];
        strcpy(clist[i], lit.toUtf8().data());
    }

    bool *parseSuccess = new bool();
    HsStablePtr pexpr = hsParseLiteralList(length, clist, parseSuccess);
    if (!*parseSuccess)
    {
        hs_free_stable_ptr(pexpr);
        delete parseSuccess;
        for (int i=0; i<length; i++)
            delete clist[i];
        delete[] clist;
        return;
    }

    HsStablePtr ptype = hsMaybeParseType(const_cast<char *>(type.toUtf8().constData()));
    HsStablePtr cell = hsMkCell(name.toUtf8().data(), ptype, pexpr);
    hsInsert(key, cell, hsSheet);

    hs_free_stable_ptr(cell);
    hs_free_stable_ptr(ptype);
    hs_free_stable_ptr(pexpr);
    delete parseSuccess;
    for (int i=0; i<length; i++)
        delete clist[i];
    delete[] clist;

    hsEvalSheet(hsSheet);
    emit reevaluated();
}

std::variant<std::monostate, QString, HsValue> HsSheet::queryCell(int key)
{
    std::variant<std::monostate, QString, HsValue> retval {};

    bool *qSuccess = new bool();
    HsStablePtr value = hsQuery(key, hsSheet, qSuccess);
    if (!*qSuccess) goto cleanup;

    if (auto error = (char *) hsDisplayError(value); *error)
        retval.emplace<1>(QString::fromUtf8(error));
    else
        retval.emplace<2>(HsValue(hsExtractValue(value), static_cast<HsValue::ValueType>(hsExtractTopLevelType(value))));

cleanup:
    hs_free_stable_ptr(value);
    delete qSuccess;
    return retval;
}