#include "tabledelegate.h"

#include <QPainter>

void TableDelegate::paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    if (index.data().canConvert<InferredType>())
    {
        QString typeText = qvariant_cast<InferredType>(index.data()).type;

        if (option.state & QStyle::State_Selected)
            painter->fillRect(option.rect, option.palette.highlight());

        painter->save();

        // leading italic correction
        painter->translate(2,0);

        QColor placeholderColor = option.palette.placeholderText().color();
        QPen pen(placeholderColor);
        painter->setPen(pen);

        QFont inferredTypeFont = QFont(option.font);
        inferredTypeFont.setItalic(true);
        painter->setFont(inferredTypeFont);

        QString elidedText = QFontMetrics(inferredTypeFont).elidedText(typeText, Qt::ElideRight, option.rect.width());
        painter->drawText(option.rect, Qt::AlignLeft | Qt::AlignVCenter, elidedText);

        painter->restore();
    }
    else
        QStyledItemDelegate::paint(painter, option, index);
}
