#ifndef TABLE_H
#define TABLE_H

#include <QWidget>

class HsCell;
class HsSheet;
class TableModel;
class QTableView;
class QLineEdit;

class Table : public QWidget
{
    Q_OBJECT
public:
    explicit Table(int key, HsSheet *sheet, QWidget *parent = nullptr);

    void loadValueFrom(const HsCell &cell);

protected:
    void contextMenuEvent (QContextMenuEvent *event) override;

private slots:
    void addRow();
    void addCol();

private:
    TableModel *model;

    QLineEdit *nameEdit;
    QTableView *valueDisplay;

    QAction *addRowAct;
    QAction *addColAct;
};

#endif // TABLE_H
