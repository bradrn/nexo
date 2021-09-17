#ifndef INPUTLIST_H
#define INPUTLIST_H

#include <QWidget>

class HsSheet;
class QLineEdit;
class QListView;
class QStringListModel;

class InputList : public QWidget
{
    Q_OBJECT
public:
    explicit InputList(int key, HsSheet *sheet, QWidget *parent = nullptr);

public slots:
    void invalidate();
    void addItem();

private:
    int key;
    HsSheet *sheet;

    QLineEdit *nameEdit;
    QLineEdit *typeEdit;
    QStringListModel *model;
    QListView *valueDisplay;
};

#endif // INPUTLIST_H
