#ifndef HSTABLEMODEL_H
#define HSTABLEMODEL_H

#include <QAbstractTableModel>

class HsCell;
class HsValue;
class HsSheet;
class QLineEdit;

class TableModel : public QAbstractTableModel
{
    Q_OBJECT
public:
    TableModel(int key, HsSheet *sheet, QObject *parent = nullptr);
    void setName(QString name);

    void loadValueFrom(const HsCell &cell);

    int rowCount(const QModelIndex &parent = QModelIndex()) const override;
    int columnCount(const QModelIndex &parent = QModelIndex()) const override;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;
    Qt::ItemFlags flags(const QModelIndex &index) const override;
    bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole) override;
    bool insertRows(int row, int count, const QModelIndex &parent = QModelIndex()) override;
    bool insertColumns(int column, int count, const QModelIndex &parent = QModelIndex()) override;

public slots:
    void invalidate();
    void requery();

private:
    int key;
    QString name;
    HsSheet *sheet;

    static const int prefaceRows = 3;
    struct Column
    {
        QString header;
        // below field are optional: either a QString or nullptr
        QString *type;
        QString *formula;
        QStringList cells;
    };

    QMap<int, Column> contents;
    QHash<QString, QVector<HsValue *>> values;

    int rows = 128;
    int cols = 128;

    void doSetData(const QModelIndex &index, const QVariant &value);
};

#endif // HSTABLEMODEL_H
