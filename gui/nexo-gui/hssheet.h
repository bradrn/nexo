#ifndef HSSHEET_H
#define HSSHEET_H

#include <HsFFI.h>
#include <QWidget>
#include <optional>
#include <variant>

class HsCell;
class HsValue;
class QString;

class HsSheet : public QWidget
{
    Q_OBJECT
public:
    HsSheet(std::optional<QString> directory = std::optional<QString>());
    ~HsSheet();

    HsSheet(const HsSheet&) = delete;
    HsSheet operator=(const HsSheet&) = delete;

    static std::optional<HsSheet *> parse(QString input);

    QString render();

    // Insert various types of cells into a sheet. Note that these trigger reevaluation.
    void insertCell(int key, QString name, QString type, QString expr);
    void insertLiteralList(int key, QString name, QString type, QStringList lits);
    void insertTable(
            int key,
            QString name,
            QStringList headers,
            QVector<QString *> formulae,
            QVector<QStringList> columns);

    /* Turn all 'insertXXX' methods into noops. Useful when creating cells from
     * pre-existing values, to make sure nothing is overwritten.
     */
    void disableInserts();
    void enableInserts();

    std::variant<std::monostate, QString, HsValue> queryCell(int key);

    bool reevaluate();

    QHash<int, HsCell *> cells();

signals:
    void reevaluated();

private:
    HsSheet(HsStablePtr hsSheet);

    HsStablePtr hsSheet;
    bool disallowInserts = false;
    std::optional<QString> directory;
};

#endif // HSSHEET_H
