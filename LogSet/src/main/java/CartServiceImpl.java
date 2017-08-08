package com.memebox.iampay.api.cart.impl;

// TODO: CartConfig 변경
//import com.memebox.iampay.api.config.CartConfig;
import com.memebox.iampay.api.config.CartConfig;
import com.memebox.iampay.api.param.APICartReq;
import com.memebox.iampay.api.param.APIUserReq;
import com.memebox.iampay.api.param.ResponseObj;
import com.memebox.iampay.api.param.ItemGroupingList;
import com.memebox.iampay.api.cart.CartService;
import com.memebox.iampay.api.cart.ItemGroupListService;
import com.memebox.iampay.api.cart.ItemService;

import com.memebox.iampay.repository.CartRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import com.memebox.iampay.entity.Cart;
import com.memebox.iampay.entity.Item;

import javax.inject.Inject;
import java.util.*;

/**
 * Created by yjlee on 15. 7. 27..
 */

@Service
public class CartServiceImpl implements CartService {
    private Logger logger = LoggerFactory.getLogger(this.getClass());

    @Inject
    ItemService itemService;

    @Inject ItemGroupListService itemGroupListService;

    @Inject
    CartRepository cartRepository;

    @Override
    public Boolean validate(APICartReq cart) {
        if (cart.getItemId() <= 0) {
            return false;
        }

        if (cart.getItemCount() == 0) {
            return false;
        }

        return true;
    }

    /**
     * CREATE
     */

    @Override
    public ResponseObj createCarts(List<APICartReq> carts, APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();
        List<ResponseObj> responseObjs = new ArrayList<>();

        if (carts == null) {
            responseObj.setIsNullError("carts");

            return responseObj;
        }

        if (! user.validate()) {
            responseObj.setIsNullError("userId");
            return  responseObj;
        }

        for (APICartReq c : carts) {
            ResponseObj r = new ResponseObj();

            if (! validate(c)) {
                r.setError(HttpStatus.NO_CONTENT, "cart is not valid");
                r.setData(c);
                responseObjs.add(r);
            } else {
                responseObjs.add(createCart(c, user));
            }
        }

        responseObj.setData(responseObjs);
        responseObj.setOkStatus();

        return responseObj;
    }

    //구매 한정 체크 로직 추후 추가
    private ResponseObj createCart(APICartReq cartReq, APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();
        List<Cart> oldNormalCarts ;

        ResponseObj loadAndCheckItem = loadAndCheckItem(cartReq);

        if (loadAndCheckItem.getStatus() != HttpStatus.OK.value()) {
            return loadAndCheckItem;
        }

        Item item = (Item) loadAndCheckItem.getData();

        if (user.isMember()) {
            oldNormalCarts = cartRepository.findByUserIdAndItemAndStatus(user.getUserId(),
                    item, CartConfig.NORMAL);
        } else {
            oldNormalCarts = cartRepository.findBySessionIdAndItemAndStatus(user.getSessionId(),
                    item, CartConfig.NORMAL);
        }

        int oldNormalCartSize = oldNormalCarts.size();
        int count = 0;
        int maxItemCount = getMaxItemCount(item, cartReq.getItemCount());

        Cart cart = new Cart();

        if (oldNormalCartSize > 0) {
            for (Cart c: oldNormalCarts) {
                count++;

                if (count == oldNormalCartSize) {
                    c.setItemCount(maxItemCount);
                    c.setPlatform(user.getPlatform());
                    responseObj.setData(c);
                } else {
                    c.setStatus(CartConfig.REMOVE);
                }
                cartRepository.save(oldNormalCarts);
            }
        } else {
            if (user.isMember()) {
                cart.setUserId(user.getUserId());
            } else {
                cart.setSessionId(user.getSessionId());
            }

            cart.setStatus(CartConfig.NORMAL);
            cart.setItem(item);
            cart.setItemCount(maxItemCount);
            cart.setPlatform(user.getPlatform());
            cartRepository.save(cart);
            responseObj.setData(cart);
        }

        responseObj.setOkStatus();

        return responseObj;
    }
    /**
     * 1. 아이템을 가져온다. (상태가 판매가능, 수량이 0이상인것)
     * 2. 수량 제한 아이템이면 구매를 조회한다.
     * 3. 이전에 담긴 장바구니가 있는지 체크한다.
     * 4. 장바구니가 있으면 업데이트, 없으면 만든다.
     */
    private Cart createCart(APICartReq apiCart, APIUserReq user) throws Exception {
        Item item = loadSaleItem(cart.getItemid());
        Cart cart = new Cart();

        if (item.isNullItem()) {
            return cart;
        }

        int maxCartItemCount = getMaxCartItemCount(apiCart, user);

        if (maxCartItemCount <= 0) {
            return cart;
        }

        removeDuplicateCart(apiCart.getItemId, user);

        cart = loadCartByItemAndUser(item, user);

        cart.setItemCount(maxCartItemCount);
        cart.setItem(item);
        cart.setUser(user);

        cartRepository.save(cart);

        return cart;
    }

    private Cart loadCartByItemAndUser(Item item, APIUserReq user) {
        Cart cart;

        if (user.isMember()) {
            cart = cartRepository.findOneByUserIdAndItemAndStatus(user.getUserId(),
                    item, CartConfig.NORMAL);
        } else {
            cart = cartRepository.findOneBySessionIdAndItemAndStatus(user.getSessionId(),
                    item, CartConfig.NORMAL);
        }

        if (cart == null) {
            cart = new Cart();
        }
        return cart;
    }

    private void removeDuplicateCart(int itemId, APIUserReq user) throws Exception {
        Item item = itemService.loadItem(itemId);
        Cart carts = loadCartsByItemAndUser(item, user);
        int count = 0;
        int cartSize = carts.size();

        if (carts.size() > 1) {
            for (Cart c: carts) {
                count++;

                if (count < cartSize) {
                    c.setStatus(CartConfig.REMOVE);
                }
            }
        }

        cartRepository.save(carts);
    }

    private List<Cart> loadCartsByItemAndUser(Item item, APIUserReq user) {
        List<Cart> carts ;

        if (user.isMember()) {
            carts = cartRepository.findByUserIdAndItemAndStatus(user.getUserId(),
                    item, CartConfig.NORMAL);
        } else {
            carts = cartRepository.findBySessionIdAndItemAndStatus(user.getSessionId(),
                    item, CartConfig.NORMAL);
        }

        return carts;
    }


    private Item loadSaleItem(Long itemId, APIUserReq user) throws Exception {
        Item item = itemService.loadItem(itemId);

        if (! item.isSelling()) {
            item = new Item();
        }

        return item;
    }

    /**
     * 1. 구매 제한이면 구매이력 조
     *
     */
    private int getMaxCartItemCount(APICartReq cart, APIUserReq user) {
        int purchasableItemCount = 0;
        Item item = itemService.loadItem(cart.getItemId());
        int maxCartItemCount = item.getMaxItemCount();

        maxCartItemCount = getMaxItemCount(item, cart.getItemCount());

        if (item.isPurchaseLimitedItem()) {
            purchasableItemCount = getPurchasableItemCount(item, user);

            if (purchasableItemCount < maxCartItemCount) {
                maxCartItemCount = purchasableItemCount;
            }
        }

        return maxCartItemCount;
    }


    //item 에 추가
    private Boolean isPurchaseLimitedItem(Item item) {
        return item.purchaseLimit > 0 ? true : false;
    }

    //item 에 추갸
    public Boolean isNullItem(Item item) {
        return item.id > 0;
    }

    //Cart 에 추가
    public void setUser(APIUserReq user) {
        if (user.isMember()) {
            userId = user.getUserId();
        } else {
            sessionId = user.getSessionId();
        }
    }



    private int getPurchasableItemCount(Item item, APIUserReq user) {
        int purchasedItemCount = 0;
        int purchasableItemCount = item.getPurcaseLimit;

        if (purchasedItemCount > purchasableItemCount) {
            return 0;
        }

        return purchasableItemCount - purchasedItemCount;
    }





    /**
     * READ
     */

    //ok
    @Override
    public ResponseObj loadCartAndItem(Long cartId) {
        ResponseObj responseObj = new ResponseObj();

        if (cartId <= 0) {
            responseObj.setIsNullError("cartId");
            return responseObj;
        }

        Cart cart = cartRepository.findById(cartId);

        if (cart == null) {
            responseObj.setError(HttpStatus.NO_CONTENT, "can't find cart");
            return responseObj;
        }

        responseObj.setOkStatus();
        responseObj.setData(cart);

        return responseObj;
    }

    /**
     * 1. 아이템 상태 체크, 수량 체크 없으면 삭제, 수량 업데이트
     * 2. 딜리버리 그룹 생성
     */

    @Override
    public ResponseObj loadCartGroup(APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();

        if (! user.validate()) {
            responseObj.setIsNullError("userId");
            return  responseObj;
        }

        List<Cart> carts;

        if (user.isMember()) {
            carts = cartRepository.findByUserIdAndStatus(user.getUserId(), CartConfig.NORMAL);
        } else {
            carts = cartRepository.findBySessionIdAndStatus(user.getSessionId(), CartConfig.NORMAL);
        }

        HashMap<String, List<Cart>> cartMap = new HashMap<>();

        cartMap.put("remove", removeNotSaleCart(carts));
        cartMap.put("adjust", adjustCartItemCount(carts));

        if (user.isMember()) {
            cartMap.put("normal", cartRepository.findByUserIdAndStatus(user.getUserId(),
                    CartConfig.NORMAL));
        } else {
            cartMap.put("normal",
                    cartRepository.findBySessionIdAndStatus(user.getSessionId(), CartConfig.NORMAL));
        }

        List<ItemGroupingList> itemGroupingLists = itemGroupListService.makeDelivery(cartMap.get("normal"));

        responseObj.setData(itemGroupingLists);
        responseObj.setOkStatus();

        return responseObj;
    }

    private List<Cart> removeNotSaleCart(List<Cart> carts) {
        List<Cart> removedCarts = new ArrayList<>();

        for (Cart c : carts) {
            Item item = c.getItem();

            if(!item.isSelling()) {
                c.setStatus(CartConfig.REMOVE);
                removedCarts.add(c);
            }
        }

        cartRepository.save(removedCarts);

        return removedCarts;
    }

    private List<Cart> adjustCartItemCount(List<Cart> carts) {
        List<Cart> adjustCarts = new ArrayList<>();

        for (Cart c : carts) {
            Item item = c.getItem();
            int maxItemCount = item.getMaxItemCount();

            if(item.isSelling() && maxItemCount < c.getItemCount()) {
                c.setItemCount(maxItemCount);
                adjustCarts.add(c);
            }
        }

        cartRepository.save(adjustCarts);

        return adjustCarts;
    }

    @Override
    public ResponseObj calculate(List<APICartReq> carts, APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();
        List<Cart> newCart = new ArrayList();

        if (! user.validate()) {
            responseObj.setIsNullError("userId");
            return  responseObj;
        }

        for(APICartReq c : carts) {
            Cart cart = cartRepository.findById(c.getId());

            if (cart != null) {
                newCart.add(cart);
            }
        }

        List<ItemGroupingList> itemGroupingLists = itemGroupListService.makeDelivery(newCart);

        responseObj.setData(itemGroupingLists);
        responseObj.setOkStatus();

        return responseObj;
    }

    @Override
    public ResponseObj loadCartCount(APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();

        if (! user.validate()) {
            responseObj.setIsNullError("userId");
            return  responseObj;
        }

        List<Cart> carts ;

        if (user.getType().equals("userId")) {
            carts = cartRepository.findByUserIdAndStatus(user.getUserId(), CartConfig.NORMAL);
        } else {
            carts = cartRepository.findBySessionIdAndStatus(user.getSessionId(), CartConfig.NORMAL);
        }

        int cartCount = carts.size();

        responseObj.setData(cartCount);
        responseObj.setOkStatus();

        return responseObj;
    }

    /**
     * UPDATE
     */

    @Override
    public HashMap<String, List<Cart>> adjustCarts(List<Cart> carts, APIUserReq user) {
        HashMap<String, List<Cart>> cartMap = new HashMap<>();

        cartMap.put("remove", removeNotSaleCart(carts));
        cartMap.put("adjust", adjustCartItemCount(carts));

        if (user.isMember()) {
            cartMap.put("normal", cartRepository.findByUserIdAndStatus(user.getUserId(),
                    CartConfig.NORMAL));
        } else {
            cartMap.put("normal",
                    cartRepository.findBySessionIdAndStatus(user.getSessionId(), CartConfig.NORMAL));
        }

        cartRepository.save(carts);

        return cartMap;
    }

    //check: maxItemCount, item.status
    @Override
    public ResponseObj updateCartAndUser(APICartReq cart, APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();

        if (! user.validate()) {
            responseObj.setIsNullError("userId");
            return  responseObj;
        }

        Cart c;
        if (user.getType().equals("userId")) {
            c = cartRepository.findByIdAndUserId(cart.getId(), user.getUserId());
        } else {
            c = cartRepository.findByIdAndSessionId(cart.getId(), user.getSessionId());
        }

        int maxItemCount = c.getItem().getMaxItemCount();

        if (maxItemCount < c.getItemCount()) {
            c.setItemCount(maxItemCount);
        } else {
            c.setItemCount(c.getItemCount());
        }

        c.setPlatform(cart.getPlatform());
        cartRepository.save(c);

        responseObj.setData(c);
        responseObj.setOkStatus();

        return responseObj;
    }

    @Override
    public ResponseObj updateCartsAndUser(List<APICartReq> carts, APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();
        List<ResponseObj> responseObjs = new ArrayList<>();

        if (! user.validate()) {
            responseObj.setIsNullError("userId");
            return  responseObj;
        }

        for (APICartReq c : carts) {
            ResponseObj r = updateCartAndUser(c, user);

            responseObjs.add(r);
        }

        responseObj.setData(responseObjs);
        responseObj.setOkStatus();

        return responseObj;
    }

    private ResponseObj updateCart(APICartReq cart) {
        ResponseObj responseObj = new ResponseObj();
        Cart oldCart = null;

//        if (cart == null) {
//            responseObj.setIsNullError("cart");
//            return responseObj;
//        } else if ( !validate(cart)) {
//            responseObj.setIsNotValidError("Cart");
//            return responseObj;
//        }

        //     oldCart = cartRepository.findById(cart.getId());

        if (oldCart == null) {
            responseObj.setIsNullError("cart");
            return responseObj;
        }

        ResponseObj setAndCheckItem = loadAndCheckItem(cart);

        if (setAndCheckItem.getStatus() != HttpStatus.OK.value()) {
            return setAndCheckItem;
        }

//        int maxItemCount = getMaxItemCount(cart, 0);
        int maxItemCount = 0;

        if (maxItemCount < cart.getItemCount()) {
            responseObj.setError(HttpStatus.NO_CONTENT, "over max item count");
            return responseObj;
        }

        cart.setItemCount(maxItemCount);

        //cartDao.updateCart(cart);

        responseObj.setData(cart);
        responseObj.setOkStatus();

        return responseObj;

    }

    //기존 장바구니 중 게스트 장바구니와 같은 아이템이 있으면 merge 변경
    //게스트 장바구니에 유저 정보 업데이트
    @Override
    public ResponseObj loginCart(APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();

        if (user.getSessionId().equals("") || user.getUserId() <= 0) {
            responseObj.setIsNullError("user");
            return responseObj;
        }

        List<Cart> memberCarts =
                cartRepository.findByUserIdAndStatus(user.getUserId(), CartConfig.NORMAL);
        List<Cart> guestCarts =
                cartRepository.findBySessionIdAndStatus(user.getSessionId(), CartConfig.NORMAL);
        Long userId = user.getUserId();

        for (Cart gc : guestCarts) {
            Long itemId = gc.getItemId();

            for (Cart mc : memberCarts) {
                if (itemId == mc.getItemId()) {
                    mc.setStatus(CartConfig.MERGE);
                }
            }

            gc.setUserId(userId);
        }

        cartRepository.save(memberCarts);
        cartRepository.save(guestCarts);

        responseObj.setOkStatus();

        return responseObj;
    }

    /**
     * DELETE
     */

    @Override
    public ResponseObj removeCart(APICartReq cart, APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();

        if (cart == null) {
            responseObj.setIsNullError("cart");
            return responseObj;
        }

        Cart oldCart;

        if (user.getType().equals("userId")) {
            oldCart = cartRepository.findByIdAndUserId(cart.getId(), user.getUserId());
        } else {
            oldCart = cartRepository.findByIdAndSessionId(cart.getId(), user.getSessionId());
        }

        if (oldCart == null) {
            responseObj.setIsNotValidError("Cart");
            return responseObj;
        }

        oldCart.setStatus(CartConfig.REMOVE);
        cartRepository.save(oldCart);

        responseObj.setData(oldCart);
        responseObj.setOkStatus();

        return responseObj;
    }

    @Override
    public ResponseObj removeCarts(List<APICartReq> carts, APIUserReq user) {
        ResponseObj responseObj = new ResponseObj();
        List<ResponseObj> responseObjs = new ArrayList<>();

        for (APICartReq c : carts) {
            responseObjs.add(removeCart(c, user));
        }

        responseObj.setOkStatus();
        responseObj.setData(responseObjs);

        return responseObj;
    }

    private int getMaxItemCount(Item item, int cartItemCount) {
        int maxItemCount = item.getMaxItemCount();

        if (cartItemCount > maxItemCount) {
            return maxItemCount;
        } else {
            return cartItemCount;
        }
    }

    private ResponseObj loadAndCheckItem(APICartReq cart) {
        ResponseObj responseObj = new ResponseObj();

        Item item = itemService.loadItem(cart.getItemId());

        if (item == null) {
            responseObj.setIsNotValidError("cart.itemId");
            return responseObj;
        } else if (! item.isSelling()) {
            responseObj.setError(HttpStatus.NOT_ACCEPTABLE, "Item is not selling");
            responseObj.setData(cart);

            return responseObj;
        }

        responseObj.setData(item);
        responseObj.setOkStatus();

        return responseObj;
    }
}
