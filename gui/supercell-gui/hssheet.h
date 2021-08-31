#ifndef HSSHEET_H
#define HSSHEET_H

#include <HsFFI.h>
#include <QWidget>
#include <variant>

class HsValue;
class QString;

class HsSheet : public QWidget
{
    Q_OBJECT
public:
    HsSheet();
    ~HsSheet();

    HsSheet(const HsSheet&) = delete;
    HsSheet operator=(const HsSheet&) = delete;

    void insertCell(int key, QString name, QString type, QString expr);
    std::variant<std::monostate, QString, HsValue> queryCell(int key);

signals:
    void reevaluated();

private:
    HsStablePtr hsSheet;
};

#endif // HSSHEET_H
