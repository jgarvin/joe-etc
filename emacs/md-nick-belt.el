(require 'md-belt-impl)

(setq md-nick-belt
        (make-md-belt
         :name "nick"
         :construct nil
         :destruct nil
         :contents 'md-active-erc-nicknames
         :context '(md-channel-buffer-p)
         :color "orange"))

(add-to-list 'md-belt-list md-nick-belt)

