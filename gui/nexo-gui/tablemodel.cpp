#include "tablemodel.h"
#include "hssheet.h"
#include "hsvalue.h"
#include "hscell.h"
#include "tabledelegate.h"

#include <QLineEdit>

TableModel::TableModel(int key, HsSheet *sheet, QObject *parent)
    : QAbstractTableModel(parent)
    , key(key)
    , sheet(sheet)
{
    // initialise table data
    contents = QMap<int, Column>();
    contents.insert(0,
        { "Column1",
          nullptr,
          nullptr,
          QStringList("0")
        });

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

    contents = QMap<int, Column>();

    for (int col=0; col<cols; ++col)
        for (int row=0; row<rows; ++row)
            doSetData(createIndex(row, col), cell.exprAt(row, col));

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

    if (!contents.contains(index.column()))
        return QVariant();

    Column column = contents.value(index.column());

    if (role == Qt::DisplayRole || role == Qt::EditRole)
    {
        if (index.row() == 0)
            return column.header;
        if (index.row() == 1)
        {
            if (QString *type = column.type)
                return *type;
            else if (HsCell *c = sheet->queryCell(key))
            {
                return QVariant::fromValue(TableDelegate::InferredType { c->typeOf(column.header) });
            }
            else
                return QVariant();
        }
        if (index.row() == 2)
        {
            if (QString *formula = column.formula)
                return *formula;
            else
                return QVariant();
        }
    }

    int itemIndex = index.row() - prefaceRows;
    if (role == Qt::DisplayRole)
    {
        QVector<HsValue *> columnValues = values.value(column.header);
        if (itemIndex < columnValues.count())
            return columnValues[itemIndex]->render();
    }
    else if (role == Qt::EditRole)
    {
        if (itemIndex < column.cells.count())
            return column.cells[itemIndex];
    }

    return QVariant();
}

QVariant TableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role != Qt::DisplayRole)
        return QVariant();

    if (orientation == Qt::Horizontal)
    {
        if (contents.contains(section))
            return contents.value(section).header;
        else
            return QVariant();
    }

    switch(section)
    {
    case 0:
        return tr("Header");
    case 1:
        return tr("Type");
    case 2:
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
    QStringList headers;
    QVector<QString *> types;
    QVector<QString *> formulae;
    QVector<QStringList> cells = QVector<QStringList>();

    int realColumnCount = 1 + *std::max_element(contents.keyBegin(), contents.keyEnd());
    headers.reserve(realColumnCount);
    types.reserve(realColumnCount);
    formulae.reserve(realColumnCount);
    cells.reserve(realColumnCount);

    for (auto i = contents.constBegin(); i != contents.constEnd(); ++i)
    {
        const Column &column = *i;
        headers.append(column.header);
        types.append(column.type);
        formulae.append(column.formula);
        cells.append(column.cells);
    }
    sheet->insertTable(key, name, headers, types, formulae, cells);
}

void TableModel::requery()
{
    auto result = sheet->queryValue(key);

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
    bool neednewheader = false;
    if (!contents.contains(index.column()))
    {
        contents.insert(index.column(),
            { "",
              nullptr,
              nullptr,
              QStringList("")
            });
        neednewheader = true;
    }

    Column &column = contents[index.column()];

    // replace header if needed

    if (index.row() == 0)
    {
        column.header = value.toString();
        return;
    }
    if (neednewheader)
        setData(createIndex(0, index.column()), tr("Column%1").arg(index.column()+1));

    // replace content

    if (index.row() == 1)
    {
        QString valueStr = value.toString();
        if (valueStr.isEmpty())
            column.type = nullptr;
        else
            column.type = new QString(valueStr);

    }
    else if (index.row() == 2)
    {
        QString valueStr = value.toString();
        if (valueStr.isEmpty())
            column.formula = nullptr;
        else
            column.formula = new QString(valueStr);
    }
    else
    {
        int i = index.row()-prefaceRows;
        if (i < column.cells.count())
            column.cells.replace(index.row()-prefaceRows, value.toString());
        else
        {
            column.cells.reserve(i+1);
            for (int j=column.cells.count(); j<i; ++j)
                column.cells.append("");
            column.cells.append(value.toString());
        }
    }
}
