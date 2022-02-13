#include "tablemodel.h"
#include "hssheet.h"
#include "hsvalue.h"
#include "hscell.h"

#include <QLineEdit>

TableModel::TableModel(int key, HsSheet *sheet, QObject *parent)
    : QAbstractTableModel(parent)
    , key(key)
    , sheet(sheet)
{
    // initialise table data
    headers = QStringList("Column1");
    formulae = QVector<QString *>(1, nullptr);
    columns = QMap<int,QStringList>({std::pair(0, QStringList("0"))});

    connect(sheet, &HsSheet::reevaluated, this, &TableModel::requery);

    invalidate();
}

void TableModel::setName(QString name)
{
    this->name = name;
    invalidate();
}

void TableModel::loadValueFrom(const HsCell &cell)
{
    emit layoutAboutToBeChanged();

    rows = cell.rows();
    cols = cell.cols();

    headers = QStringList();
    formulae = QVector<QString *>(cols, nullptr);
    columns = QMap<int, QStringList>({std::pair(cols, QStringList())});

    for (int col=0; col<cols; ++col)
    {
        headers.append("");
        for (int row=0; row<rows; ++row)
            doSetData(createIndex(row, col), cell.exprAt(row, col));
    }

    requery();
    // no need to change persistent indices, I think
    emit layoutChanged();
}

int TableModel::rowCount(const QModelIndex &parent) const
{
    return rows+prefaceRows;
}

int TableModel::columnCount(const QModelIndex &parent) const
{
    return cols;
}

QVariant TableModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();

    if (index.column() >= headers.count())
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

    int itemIndex = index.row() - prefaceRows;
    if (role == Qt::DisplayRole)
    {
        QString label = headers[index.column()];
        QVector<HsValue *> column = values.value(label);
        if (itemIndex < column.count())
            return column[itemIndex]->render();
    }
    else if (role == Qt::EditRole)
    {
        QStringList column = columns.value(index.column());
        if (itemIndex < column.count())
            return column[itemIndex];
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
            role == Qt::EditRole)
    {
        doSetData(index, value);

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
    ++rows;
    invalidate();
    endInsertRows();

    return true;
}

bool TableModel::insertColumns(int column, int count, const QModelIndex &parent)
{
    beginInsertColumns(QModelIndex(), column, column+count-1);
    ++cols;
    invalidate();
    endInsertColumns();

    return true;
}

void TableModel::invalidate()
{
    QVector<QStringList> denseColumns = QVector<QStringList>();
    denseColumns.reserve(headers.count());
    for (auto i = columns.constBegin(); i != columns.constEnd(); ++i)
        denseColumns.append(*i);
    sheet->insertTable(key, name, headers, formulae, denseColumns);
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

void TableModel::doSetData(const QModelIndex &index, const QVariant &value)
{
    // if column does not exist, create it
    int col = index.column();
    bool neednewheader = false;
    if (col >= headers.count())
    {
        headers.reserve(col+1);
        for (int j=headers.count(); j<=col; ++j)
            headers.append("");
        neednewheader = true;
    }
    if (col >= formulae.count())
    {
        formulae.reserve(col+1);
        for (int j=formulae.count(); j<=col; ++j)
            formulae.append(nullptr);
    }
    if (col > *std::max_element(columns.keyBegin(), columns.keyEnd()))
    {
        columns.insert(col, QStringList());
    }

    // replace header if needed

    if (index.row() == 0)
    {
        headers.replace(index.column(), value.toString());
        return;
    }
    if (neednewheader)
        setData(createIndex(0, col), tr("Column%1").arg(col+1));

    // replace content

    if (index.row() == 1)
    {
        QString valueStr = value.toString();
        if (valueStr.isEmpty())
            formulae.replace(index.column(), nullptr);
        else
            formulae.replace(index.column(), new QString(valueStr));
    }
    else
    {
        QStringList &column = columns[index.column()];
        int i = index.row()-prefaceRows;
        if (i < column.count())
            column.replace(index.row()-prefaceRows, value.toString());
        else
        {
            column.reserve(i+1);
            for (int j=column.count(); j<i; ++j)
                column.append("");
            column.append(value.toString());
        }
    }
}
