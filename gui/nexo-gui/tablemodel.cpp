#include "tablemodel.h"
#include "hssheet.h"
#include "hsvalue.h"

#include <QLineEdit>

TableModel::TableModel(int key, HsSheet *sheet, QObject *parent)
    : QAbstractTableModel(parent)
    , key(key)
    , sheet(sheet)
{
    // initialise table data
    headers = QStringList("Column1");
    formulae = QVector<QString *>(1, nullptr);
    columns = QVector<QStringList>();
    columns.append(QStringList("0"));

    connect(sheet, &HsSheet::reevaluated, this, &TableModel::requery);

    invalidate();
}

void TableModel::setName(QString name)
{
    this->name = name;
    invalidate();
}

int TableModel::rowCount(const QModelIndex &parent) const
{
    if (columns.empty()) return 1;
    return columns[0].count()+prefaceRows;
}

int TableModel::columnCount(const QModelIndex &parent) const
{
    return headers.count();
}

QVariant TableModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();

    if (index.column() >= headers.count())
        return QVariant();

    if (index.row() >= columns[0].count()+prefaceRows)
        return QVariant();

    if (role == Qt::DisplayRole || role == Qt::EditRole)
    {
        if (index.row() == 0)
            return headers[index.column()];
        if (index.row() == 1)
        {
            if (QString *formula = formulae[index.column()])
                return *formula;
            else
                return QVariant();
        }
    }

    if (role == Qt::DisplayRole)
    {
        QString label = headers[index.column()];
        QVector<HsValue *> column = values[label];
        int itemIndex = index.row() - prefaceRows;
        if (itemIndex < column.count())
            return column[itemIndex]->render();
    }
    else if (role == Qt::EditRole)
    {
        return columns[index.column()][index.row()-prefaceRows];
    }

    return QVariant();
}

QVariant TableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role != Qt::DisplayRole)
        return QVariant();

    if (orientation == Qt::Horizontal)
        if (section < headers.count())
            return headers[section];

    switch(section)
    {
    case 0:
        return tr("Header");
    case 1:
        return tr("Formula");
    default:
        return QStringLiteral("%1").arg(section-prefaceRows);
    }
}

Qt::ItemFlags TableModel::flags(const QModelIndex &index) const
{
    if (!index.isValid())
        return Qt::ItemIsEnabled;

    return QAbstractItemModel::flags(index) | Qt::ItemIsEditable;
}

bool TableModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
    if (index.isValid() &&
            role == Qt::EditRole &&
            index.column() < headers.count() &&
            index.row() < columns[0].count()+prefaceRows)
    {
        if (index.row() == 0)
            headers.replace(index.column(), value.toString());
        else if (index.row() == 1)
        {
            QString valueStr = value.toString();
            if (valueStr.isEmpty())
                formulae.replace(index.column(), nullptr);
            else
                formulae.replace(index.column(), new QString(valueStr));
        }
        else
            columns[index.column()].replace(index.row()-prefaceRows, value.toString());

        invalidate();
        emit dataChanged(index, index, {role});
        return true;
    }
    return false;
}

bool TableModel::insertRows(int row, int count, const QModelIndex &parent)
{
    if (row<prefaceRows)
        return false;

    beginInsertRows(QModelIndex(), row, row+count-1);

    for (int i=0; i<headers.count(); ++i)
    {
        QStringList &column = columns[i];
        for (int j=0; j<count; ++j)
            column.insert(row-prefaceRows, "");
    }

    invalidate();

    endInsertRows();
    return true;
}

bool TableModel::insertColumns(int column, int count, const QModelIndex &parent)
{
    beginInsertColumns(QModelIndex(), column, column+count-1);

    for (int i=column; i<column+count; ++i)
    {
        headers.insert(i, tr("Column%1").arg(i+1));
        formulae.insert(i, nullptr);

        int collen = columns[0].count();
        QStringList newcol = QStringList();
        for (int j=0; j<collen; ++j)
            newcol.append("");
        columns.insert(i, newcol);
    }

    invalidate();

    endInsertColumns();
    return true;
}

void TableModel::invalidate()
{
    sheet->insertTable(key, name, headers, formulae, columns);
}

void TableModel::requery()
{
    auto result = sheet->queryCell(key);

    if (QString *msg = std::get_if<QString>(&result))
    {
        //TODO figure out how to respond to this case
    }
    else if (HsValue *value = std::get_if<HsValue>(&result))
    {
        // assume this can only be a table, don't need to check type
        values = value->toTable();
    }
}
